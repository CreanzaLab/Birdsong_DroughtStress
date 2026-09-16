"""Build the syllable atlas.

    python -m atlas.build            # all bouts (resumable; skips bouts already in atlas_data/cache)
    python -m atlas.build --limit 40 # quick test
    python -m atlas.build --force    # recompute everything

For every Chipper gzip with a matching wav:
  1. load the wav, resample to config.SR, write atlas_data/audio/<bout>.wav (16-bit mono)
  2. align Chipper's pixel onsets/offsets to audio time (padding split evenly, see chipper.py)
     and QA the alignment (energy contrast, best-shift search, length check)
  3. render atlas_data/img/<bout>.png (1 px per STFT frame, no margins)
  4. cut each syllable and compute features (features.py) + Chipper frequency bounds
Then join the recording metadata and the 2022 syllable/bout tables and write
  atlas_data/syllables.csv, syllables.json, bouts.json, meta.json, qa_bouts.csv, qa_report.md
"""
from __future__ import annotations

import argparse
import csv
import json
import math
import os
import sys
import time
from collections import Counter, defaultdict
from multiprocessing import Pool

import numpy as np

from . import config
from .chipper import align, load_gzip, norm_key, norm_rec
from .viterbi import peak_metrics

csv.field_size_limit(10**9)

TABLE_NUMERIC = {          # 2022-11-15 syllable table column -> atlas column
    "All_Syllables_syllable_duration.ms.": "table_duration_ms",
    "All_Syllables_sylls_freq_modulation.Hz.": "table_freq_mod_hz",
    "All_Syllables_sylls_Upper_Freq_.Hz.": "table_upper_freq_hz",
    "All_Syllables_sylls_Lower_Freq_.Hz.": "table_lower_freq_hz",
    "nNotes": "table_n_notes",
}
TABLE_CATEG = {
    "Clust20220619.Overall": "cluster_2022",
    "removedAsWhistle": "removed_as_whistle",
    "ProbableWhistle": "probable_whistle",
}
META_CATEG = {  # metadata CSV column -> atlas column
    "Source": "source", "Era": "era", "RegionPostAug22": "region", "Region": "region_orig",
    "State": "state", "County": "county", "year": "year", "Recordist": "recordist", "UsedData": "used_data",
}

GROUPS = [
    ("Original 2022 analysis", ["table_duration_ms", "table_freq_mod_hz", "table_upper_freq_hz", "table_lower_freq_hz", "table_n_notes"]),
    ("Chipper (from gzip)", ["chipper_duration_ms", "chipper_upper_freq_hz", "chipper_lower_freq_hz", "chipper_freq_range_hz"]),
    ("Spectral", ["peak_frequency_hz", "mean_frequency_hz", "freq_lo_5pct_hz", "freq_hi_95pct_hz", "spectral_centroid",
                  "spectral_bandwidth", "spectral_rolloff", "spectral_flatness", "spectral_flux_mean"]),
    ("Pitch", ["f0_median", "f0_min", "f0_max", "f0_std", "pitch_goodness"]),
    ("Entropy & modulation", ["spectral_entropy", "temporal_entropy", "wiener_entropy", "am_mean", "rms_mean", "attack_time"]),
    ("Viterbi peak track (newFM)", ["vit_peak_freq_med_hz", "vit_peak_freq_max_hz", "vit_peak_freq_min_hz", "vit_peak_bandwidth_hz",
                                    "vit_fm_raw_khz_s", "vit_fm_filtered_khz_s"]),
    ("UMAP (see umap_meta.json for inputs)", ["umap_1", "umap_2", "umap3_1", "umap3_2", "umap3_3"]),
    ("Position in bout", ["syll_num", "n_sylls_in_bout", "rel_position", "onset_ms", "offset_ms", "gap_before_ms", "gap_after_ms",
                          "bout_duration_ms", "duration_ms"]),
    ("Location & time", ["latitude", "longitude", "year_num"]),
    ("Chipper bout settings", ["bout_hpf_hz", "bout_lpf_hz"]),
]
CATEGORICAL = ["recording", "bout_key", "source", "era", "region", "era_region", "region_orig", "state", "county", "year", "recordist",
               "cluster_2022", "syllable_pattern_id", "in_final_table", "removed_as_whistle", "probable_whistle",
               "used_data", "qa_ok", "qa_flags"]


# --------------------------------------------------------------------------- #
def _read_csv(p):
    with open(p, newline="", encoding="utf-8", errors="replace") as fh:
        return list(csv.DictReader(fh))


def _num(v):
    try:
        f = float(v)
        return f if math.isfinite(f) else None
    except (TypeError, ValueError):
        return None


def _clean(v):
    """NaN/inf -> None so the JSON is strict and CSV cells are empty."""
    if isinstance(v, float) and not math.isfinite(v):
        return None
    if isinstance(v, dict):
        return {k: _clean(x) for k, x in v.items()}
    if isinstance(v, list):
        return [_clean(x) for x in v]
    return v


def _wav_index():
    return {norm_key(f)[1]: config.WAV_DIR / f for f in os.listdir(config.WAV_DIR) if f.endswith(".wav")}


# --------------------------------------------------------------------------- #
def process_bout(job):
    """Worker: one gzip -> cache JSON (audio + png written as side effects)."""
    gz_name, wav_path = job
    import librosa
    import soundfile as sf
    from . import features as F
    from .render import px_per_ms, render_bout, render_chipper

    rec, key = norm_key(gz_name)
    cache = config.CACHE_DIR / f"{key}.json"
    try:
        bout = load_gzip(config.GZIP_DIR / gz_name)
        y_native, sr_native = sf.read(str(wav_path), dtype="float32", always_2d=True)
        y_native = y_native.mean(axis=1)
        n_native = int(y_native.size)
        y = y_native if sr_native == config.SR else librosa.resample(y_native, orig_sr=sr_native, target_sr=config.SR)
        peak = float(np.max(np.abs(y))) if y.size else 0.0
        if peak > 0:
            y = y / peak * 0.9
        al = align(bout, y, sr_native, n_native)
        # Chipper's bounding box in frequency: FrequencyFilter = [high-pass row, low-pass row] counted from 0 Hz
        from scipy.signal import butter, sosfiltfilt
        ff = bout.params.get("FrequencyFilter", [0, bout.n_rows])
        hpf_hz = max(float(ff[0]) * bout.hz_per_px, config.FEATURE_HIGHPASS_HZ)
        lpf_hz = min(float(ff[1]) * bout.hz_per_px, config.SR / 2 * 0.98)
        if lpf_hz <= hpf_hz + 100:
            lpf_hz = config.SR / 2 * 0.98
        if lpf_hz < config.SR / 2 * 0.98:
            sos = butter(4, [hpf_hz, lpf_hz], btype="bandpass", fs=config.SR, output="sos")
        else:
            sos = butter(4, hpf_hz, btype="highpass", fs=config.SR, output="sos")
        y_hp = sosfiltfilt(sos, y).astype(np.float32)          # features only; saved audio/PNG stay unfiltered

        sf.write(str(config.AUDIO_DIR / f"{key}.wav"), y, config.SR, subtype="PCM_16")
        w, h = render_bout(y, config.SR, config.IMG_DIR / f"{key}.png")
        render_chipper(bout.sonogram, bout.ms_per_px, bout.hz_per_px, al.shift_ms, y.size / config.SR * 1000,
                       w, config.IMG_DIR / f"{key}_chipper.png")

        up_hz, lo_hz = bout.syll_freq_bounds()
        dur_ms = y.size / config.SR * 1000
        sylls = []
        n = bout.n_sylls
        for i in range(n):
            a, b = al.onsets_ms[i], al.offsets_ms[i]
            ia, ib = max(0, int(round(a / 1000 * config.SR))), min(y.size, int(round(b / 1000 * config.SR)))
            clip = y_hp[ia:ib] if ib > ia else np.zeros(config.N_FFT, dtype=np.float32)
            feats = F.compute(clip, config.SR, band_hz=(hpf_hz, lpf_hz))
            feats.update(peak_metrics(clip, config.SR, _num(lo_hz[i]), _num(up_hz[i])))
            feats.update({
                "syll_num": i + 1, "n_sylls_in_bout": n, "rel_position": (i / (n - 1)) if n > 1 else 0.0,
                "onset_ms": float(a), "offset_ms": float(b),
                "gap_before_ms": float(a - al.offsets_ms[i - 1]) if i > 0 else None,
                "gap_after_ms": float(al.onsets_ms[i + 1] - b) if i < n - 1 else None,
                "chipper_duration_ms": float((bout.offsets_px[i] - bout.onsets_px[i]) * bout.ms_per_px),
                "chipper_upper_freq_hz": _num(up_hz[i]), "chipper_lower_freq_hz": _num(lo_hz[i]),
                "chipper_freq_range_hz": _num(up_hz[i] - lo_hz[i]),
            })
            sylls.append(feats)
        out = {
            "bout_key": key, "recording": rec, "bout_num": int(key.split("_bout")[1]), "gzip": gz_name,
            "wav_src": os.path.basename(str(wav_path)), "sr_native": int(sr_native), "n_native": n_native,
            "duration_ms": dur_ms, "n_sylls": n, "png": f"img/{key}.png", "png_chipper": f"img/{key}_chipper.png", "audio": f"audio/{key}.wav",
            "img_w": w, "img_h": h, "px_per_ms": px_per_ms(), "fmax_hz": config.RENDER_FMAX,
            "hpf_hz": hpf_hz, "lpf_hz": lpf_hz,
            "onsets_ms": [float(v) for v in al.onsets_ms], "offsets_ms": [float(v) for v in al.offsets_ms],
            "chipper_ms_per_px": bout.ms_per_px, "chipper_params": {k: (float(v) if isinstance(v, (int, float)) else v)
                                                                    for k, v in bout.params.items() if k != "BoutRange"},
            "qa": {"pad_samples": al.pad_samples, "shift_ms": al.shift_ms, "align_contrast_db": _num(al.contrast_db),
                   "best_shift_ms": _num(al.best_shift_ms), "flags": al.flags},
            "sylls": sylls,
        }
        with open(cache, "w") as fh:
            json.dump(out, fh, allow_nan=True)
        return key, None
    except Exception as e:  # noqa: BLE001
        return key, f"{type(e).__name__}: {e}"


# --------------------------------------------------------------------------- #
def assemble():
    meta_rows = _read_csv(config.METADATA_CSV)
    meta = {norm_rec(r["FileName"]): r for r in meta_rows}
    table = {}
    if config.SYLL_TABLE_CSV.exists():
        for r in _read_csv(config.SYLL_TABLE_CSV):
            table[(norm_key(r["File"])[1], int(float(r["SyllNum"])))] = r
    final_bouts = set()
    if config.BOUT_TABLE_CSV.exists():
        final_bouts = {norm_key(r["File"])[1] for r in _read_csv(config.BOUT_TABLE_CSV)}

    bouts, rows = {}, []
    for f in sorted(os.listdir(config.CACHE_DIR)):
        if not f.endswith(".json"):
            continue
        with open(config.CACHE_DIR / f) as fh:
            b = json.load(fh)
        key, rec = b["bout_key"], b["recording"]
        m = meta.get(rec, {})
        qa_flags = b["qa"]["flags"]
        bouts[key] = _clean({k: v for k, v in b.items() if k != "sylls"})
        bouts[key]["in_final_table"] = key in final_bouts
        bouts[key]["meta"] = {v: m.get(k, "NA") for k, v in META_CATEG.items()}
        for s in b["sylls"]:
            t = table.get((key, s["syll_num"]))
            row = {
                "id": f"{key}_s{s['syll_num']}", "bout_key": key, "recording": rec, "bout_num": b["bout_num"],
                "name": f"{rec} bout {b['bout_num']} · syll {s['syll_num']}/{s['n_sylls_in_bout']}",
                "in_final_table": str(key in final_bouts), "qa_ok": str(not qa_flags),
                "qa_flags": ",".join(qa_flags) or "none", "bout_duration_ms": b["duration_ms"],
                "latitude": _num(m.get("Latitude")), "longitude": _num(m.get("Longitude")), "year_num": _num(m.get("year")),
            }
            row.update({v: (m.get(k) or "NA") for k, v in META_CATEG.items()})
            row["era_region"] = f"{row['era']}-{row['region']}"
            row["bout_hpf_hz"] = b.get("hpf_hz"); row["bout_lpf_hz"] = b.get("lpf_hz")
            row.update(_clean(s))
            if t:
                row.update({v: _num(t[k]) for k, v in TABLE_NUMERIC.items()})
                row.update({v: (t[k] or "NA") for k, v in TABLE_CATEG.items()})
                row["syllable_pattern_id"] = t.get("syllable_pattern_ID", "NA")
            else:
                row.update({v: None for v in TABLE_NUMERIC.values()})
                row.update({v: "NA" for v in TABLE_CATEG.values()})
                row["syllable_pattern_id"] = "NA"
            rows.append(row)

    umap_path = config.DATA_DIR / "umap.csv"
    if umap_path.exists():
        um = {}
        with open(umap_path, newline="") as fh:
            for r in csv.DictReader(fh):
                um[r["id"]] = {k: _num(v) for k, v in r.items() if k != "id"}
        for r in rows:
            r.update(um.get(r["id"], {"umap_1": None, "umap_2": None, "umap3_1": None, "umap3_2": None, "umap3_3": None}))
    numeric = [c for _, cols in GROUPS for c in cols]
    fields = ["id", "name"] + CATEGORICAL + ["bout_num"] + numeric
    fields = list(dict.fromkeys(fields))
    with open(config.DATA_DIR / "syllables.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=fields, extrasaction="ignore")
        w.writeheader()
        for r in rows:
            w.writerow({k: ("" if r.get(k) is None else r.get(k)) for k in fields})
    with open(config.DATA_DIR / "syllables.json", "w") as fh:
        json.dump({"fields": fields, "rows": [[r.get(k) for k in fields] for r in rows]}, fh, allow_nan=False)
    with open(config.DATA_DIR / "bouts.json", "w") as fh:
        json.dump(bouts, fh, allow_nan=False)
    from .features import FEATURE_DOC
    umap_meta = json.load(open(config.DATA_DIR / "umap_meta.json")) if (config.DATA_DIR / "umap_meta.json").exists() else None
    with open(config.DATA_DIR / "meta.json", "w") as fh:
        json.dump({"groups": [{"name": n, "columns": c} for n, c in GROUPS], "categorical": CATEGORICAL, "umap": umap_meta,
                   "doc": FEATURE_DOC, "n_syllables": len(rows), "n_bouts": len(bouts),
                   "built": time.strftime("%Y-%m-%d %H:%M")}, fh)

    # ---- QA report ----
    qa_rows = []
    for key, b in sorted(bouts.items()):
        q = b["qa"]
        qa_rows.append({"bout_key": key, "gzip": b["gzip"], "wav_src": b["wav_src"], "sr_native": b["sr_native"],
                        "n_sylls": b["n_sylls"], "pad_samples": round(q["pad_samples"], 1), "shift_ms": round(q["shift_ms"], 2),
                        "align_contrast_db": q["align_contrast_db"], "best_shift_ms": q["best_shift_ms"],
                        "flags": ",".join(q["flags"]), "in_final_table": b["in_final_table"]})
    with open(config.DATA_DIR / "qa_bouts.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(qa_rows[0].keys()))
        w.writeheader(); w.writerows(qa_rows)
    flag_counts = Counter(f for r in qa_rows for f in r["flags"].split(",") if f)
    clean = sum(1 for r in qa_rows if not r["flags"])
    contrasts = [r["align_contrast_db"] for r in qa_rows if r["align_contrast_db"] is not None]
    devs = [abs(r["best_shift_ms"] - r["shift_ms"]) for r in qa_rows if r["best_shift_ms"] is not None]
    lines = [
        "# Syllable atlas QA report", "", f"Built {time.strftime('%Y-%m-%d %H:%M')}",
        f"Bouts: {len(qa_rows)}  ·  syllables: {len(rows)}  ·  bouts with no QA flag: {clean}",
        "", "## Alignment method",
        "Chipper's sonogram is ~3175 samples longer than the audio (STFT padding), split evenly at both ends, so",
        "`audio_ms = chipper_px * ms_per_px - pad/2`. The energy check band-limits the audio to 1.5-10 kHz and compares",
        "mean dB inside syllables vs inside the gaps between them at the derived shift, and also searches +/-100 ms for the",
        "shift that maximises that contrast.", "",
        f"- median contrast (syllable - gap): {np.median(contrasts):.1f} dB" if contrasts else "",
        f"- median |best shift - derived shift|: {np.median(devs):.1f} ms; 95th pct {np.percentile(devs, 95):.1f} ms" if devs else "",
        "", "## Flags", "",
        "| flag | bouts | meaning |", "|---|---|---|",
        f"| length_mismatch | {flag_counts.get('length_mismatch', 0)} | wav length disagrees with the gzip sonogram width: probably a different extraction of the same bout (two wavs once shared a name) |",
        f"| misaligned | {flag_counts.get('misaligned', 0)} | best energy shift differs from the derived shift by > {config.ALIGN_MAX_DEV_MS} ms |",
        f"| weak_contrast | {flag_counts.get('weak_contrast', 0)} | syllables are < {config.ALIGN_MIN_CONTRAST_DB} dB louder than the gaps (noisy recording, or wrong audio) |",
        f"| outside_audio | {flag_counts.get('outside_audio', 0)} | an onset/offset falls outside the wav |",
        "", "Flagged bouts (see qa_bouts.csv for all columns):", "",
    ]
    for r in qa_rows:
        if r["flags"]:
            lines.append(f"- {r['bout_key']}  [{r['flags']}]  pad={r['pad_samples']}  contrast={r['align_contrast_db']}  best_shift={r['best_shift_ms']}  derived={r['shift_ms']}")
    with open(config.DATA_DIR / "qa_report.md", "w") as fh:
        fh.write("\n".join(l for l in lines if l is not None))
    print(f"assembled {len(rows)} syllables / {len(bouts)} bouts -> {config.DATA_DIR}")
    print(f"QA: {clean} clean bouts; flags {dict(flag_counts)}")


# --------------------------------------------------------------------------- #
def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--limit", type=int, default=0, help="only process the first N gzips (test)")
    ap.add_argument("--force", action="store_true", help="recompute bouts already cached")
    ap.add_argument("--workers", type=int, default=config.N_WORKERS)
    ap.add_argument("--assemble-only", action="store_true", help="skip processing; just rebuild the tables from cache")
    ap.add_argument("--no-umap", action="store_true", help="skip the UMAP projection step")
    args = ap.parse_args(argv)
    for d in (config.DATA_DIR, config.IMG_DIR, config.AUDIO_DIR, config.CACHE_DIR):
        d.mkdir(parents=True, exist_ok=True)
    if not args.assemble_only:
        wavs = _wav_index()
        gzips = sorted(f for f in os.listdir(config.GZIP_DIR) if f.endswith(".gzip"))
        if args.limit:
            gzips = gzips[: args.limit]
        jobs, missing, skipped = [], [], 0
        for g in gzips:
            key = norm_key(g)[1]
            if key not in wavs:
                missing.append(g); continue
            if not args.force and (config.CACHE_DIR / f"{key}.json").exists():
                skipped += 1; continue
            jobs.append((g, wavs[key]))
        print(f"{len(gzips)} gzips: {len(jobs)} to process, {skipped} cached, {len(missing)} without a wav", flush=True)
        if missing:
            print("  missing wavs:", missing[:10], "..." if len(missing) > 10 else "")
        t0, errors = time.time(), []
        with Pool(args.workers) as pool:
            for i, (key, err) in enumerate(pool.imap_unordered(process_bout, jobs, chunksize=2), 1):
                if err:
                    errors.append((key, err))
                if i % 50 == 0 or i == len(jobs):
                    print(f"  {i}/{len(jobs)}  {time.time() - t0:.0f}s  errors={len(errors)}", flush=True)
        for k, e in errors:
            print("ERROR", k, e)
    assemble()
    if not args.no_umap:
        from .umap_map import run as run_umap
        run_umap()
        assemble()      # merge umap_1/2 (+3-D) into the tables


if __name__ == "__main__":
    sys.exit(main())
