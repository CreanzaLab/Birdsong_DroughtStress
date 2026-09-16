"""UMAP projection of the syllables (as in the original Sound Atlas, but over the
acoustic feature table rather than CLAP embeddings).

    python -m atlas.umap_map           # reads atlas_data/syllables.csv -> atlas_data/umap.csv + umap_meta.json

Exactly which variables go in is recorded in umap_meta.json and shown in the web
app. Rules: only features that exist for EVERY syllable (so no 2022-table values,
no pyin f0, no metadata, no position-in-bout), each z-scored; heavy-tailed ones are
log10-transformed first.
"""
from __future__ import annotations

import json
import sys
import time

import numpy as np
import pandas as pd

from . import config

UMAP_FEATURES = [
    "duration_ms",                                   # log10
    "chipper_upper_freq_hz", "chipper_lower_freq_hz",
    "peak_frequency_hz", "mean_frequency_hz", "freq_lo_5pct_hz", "freq_hi_95pct_hz",
    "spectral_centroid", "spectral_bandwidth", "spectral_rolloff", "spectral_flatness",
    "spectral_flux_mean",                            # log10
    "spectral_entropy", "temporal_entropy", "wiener_entropy",
    "am_mean", "pitch_goodness", "attack_time",
    "vit_peak_freq_med_hz", "vit_peak_bandwidth_hz", "vit_fm_filtered_khz_s",   # bandwidth, fm: log10(x+1)
]
LOG10 = {"duration_ms": 0.0, "spectral_flux_mean": 1e-6, "vit_peak_bandwidth_hz": 1.0, "vit_fm_filtered_khz_s": 1.0}
PARAMS = {"n_neighbors": 15, "min_dist": 0.1, "metric": "euclidean", "random_state": 0}


def run(csv_path=None, out_dir=None):
    import umap  # umap-learn

    csv_path = csv_path or config.DATA_DIR / "syllables.csv"
    out_dir = out_dir or config.DATA_DIR
    df = pd.read_csv(csv_path, low_memory=False)
    X = df[UMAP_FEATURES].astype(float).copy()
    for c, off in LOG10.items():
        X[c] = np.log10(X[c] + off)
    ok = np.isfinite(X.values).all(axis=1)
    Z = (X[ok] - X[ok].mean()) / X[ok].std(ddof=0)
    t0 = time.time()
    print(f"UMAP on {ok.sum()} syllables x {len(UMAP_FEATURES)} features ...", flush=True)
    e2 = umap.UMAP(n_components=2, **PARAMS).fit_transform(Z.values)
    e3 = umap.UMAP(n_components=3, **PARAMS).fit_transform(Z.values)
    out = pd.DataFrame({"id": df.loc[ok, "id"].values, "umap_1": e2[:, 0], "umap_2": e2[:, 1],
                        "umap3_1": e3[:, 0], "umap3_2": e3[:, 1], "umap3_3": e3[:, 2]})
    out.to_csv(out_dir / "umap.csv", index=False)
    meta = {"features": UMAP_FEATURES, "log10_transformed": list(LOG10), "scaling": "z-score per feature (mean 0, sd 1)",
            "params": PARAMS, "n_syllables": int(ok.sum()), "n_excluded_missing": int((~ok).sum()),
            "excluded_on_purpose": "2022-table values (missing for 230 bouts), pyin f0 (missing for short syllables), "
                                   "position-in-bout, recording metadata, latitude/longitude",
            "seconds": round(time.time() - t0, 1)}
    with open(out_dir / "umap_meta.json", "w") as fh:
        json.dump(meta, fh, indent=1)
    print(f"wrote umap.csv ({ok.sum()} rows) in {meta['seconds']} s")
    return meta


if __name__ == "__main__":
    sys.exit(run())
