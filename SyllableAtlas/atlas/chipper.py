"""Read Chipper SegSyllsOutput gzips, normalise file names, and align Chipper's
pixel onsets/offsets to the actual audio.

Chipper gzip layout (a pickled list of 4 dicts):
  [0] params: FrequencyFilter, BoutRange, PercentSignalKept, MinSilenceDuration, MinSyllableDuration, Normalized
  [1] {'Onsets': [px...], 'Offsets': [px...]}          columns of the sonogram
  [2] {'Sonogram': 513 x W thresholded image}          row 0 = highest frequency
  [3] {'timeAxisConversion': ms per pixel, 'freqAxisConversion': Hz per row}

Frequency of a row r is (n_rows - r) * hz_per_pixel  (matches
analyze_chipper_output_*.py: upper = first signal row, lower = last signal row + 1).
"""
from __future__ import annotations

import gzip
import os
import pickle
import re
from dataclasses import dataclass

import numpy as np

from . import config


# --------------------------------------------------------------------------- #
# Names
# --------------------------------------------------------------------------- #
def norm_key(name: str) -> tuple[str, str | None]:
    """Map any wav / gzip / table file name to (recording_id, bout_key).

    Handles every variant seen in the project: the SegSyllsOutput_ prefix, a _44k
    suffix, Melospiza-melodia- and leading '-' prefixes on Xeno-canto ids, spaces
    before the bout number, Sono.csv suffixes, _maybeIncomplete, and underscores
    inside self-recording ids (170702_0058S12 -> 1707020058S12).
    """
    n = os.path.basename(str(name)).strip()
    n = re.sub(r"\.(wav|gzip|csv)$", "", n)
    n = re.sub(r"Sono$", "", n)
    n = re.sub(r"^SegSyllsOutput_", "", n)
    n = n.replace("_maybeIncomplete", "")
    n = re.sub(r"bout\s+", "bout", n)
    n = n.replace("_44k", "")
    n = re.sub(r"^Melospiza-melodia-", "", n)
    n = re.sub(r"^-", "", n)
    m = re.match(r"^(.*?)_bout(\d+)$", n)
    rec, bout = (m.group(1), m.group(2)) if m else (n, None)
    rec = rec.replace("_", "")
    return rec, (f"{rec}_bout{bout}" if bout else None)


def norm_rec(rec: str) -> str:
    r = str(rec).strip()
    r = re.sub(r"^Melospiza-melodia-", "", r)
    r = re.sub(r"^-", "", r).replace("_", "")
    return re.sub(r"\.0$", "", r)


# --------------------------------------------------------------------------- #
# Gzip
# --------------------------------------------------------------------------- #
@dataclass
class ChipperBout:
    path: str
    onsets_px: np.ndarray
    offsets_px: np.ndarray
    ms_per_px: float
    hz_per_px: float
    n_rows: int
    width_px: int
    sonogram: np.ndarray          # uint8 (n_rows x width), 1 = signal kept by Chipper
    params: dict

    @property
    def n_sylls(self) -> int:
        return int(len(self.onsets_px))

    def syll_freq_bounds(self) -> tuple[np.ndarray, np.ndarray]:
        """Per-syllable (upper_hz, lower_hz) exactly as the original analysis script."""
        up, lo = [], []
        for a, b in zip(self.onsets_px, self.offsets_px):
            rows = np.where(self.sonogram[:, a:b].sum(axis=1) > 0)[0]
            if rows.size == 0:
                up.append(np.nan); lo.append(np.nan); continue
            up.append((self.n_rows - rows[0]) * self.hz_per_px)
            lo.append((self.n_rows - (rows[-1] + 1)) * self.hz_per_px)
        return np.array(up), np.array(lo)


def load_gzip(path) -> ChipperBout:
    with gzip.open(path, "rb") as fh:
        d = pickle.load(fh)
    sono = np.asarray(d[2]["Sonogram"])
    sono = (sono > 0).astype(np.uint8)
    return ChipperBout(
        path=str(path),
        onsets_px=np.asarray(d[1]["Onsets"], dtype=int),
        offsets_px=np.asarray(d[1]["Offsets"], dtype=int),
        ms_per_px=float(d[3]["timeAxisConversion"]),
        hz_per_px=float(d[3]["freqAxisConversion"]),
        n_rows=int(sono.shape[0]),
        width_px=int(sono.shape[1]),
        sonogram=sono,
        params=dict(d[0]),
    )


# --------------------------------------------------------------------------- #
# Alignment
# --------------------------------------------------------------------------- #
@dataclass
class Alignment:
    pad_samples: float            # sonogram length in samples - wav length in samples
    shift_ms: float               # applied: audio_ms = chipper_ms + shift_ms
    onsets_ms: np.ndarray         # in AUDIO time
    offsets_ms: np.ndarray
    contrast_db: float            # syllable-vs-gap band energy at the derived shift
    best_shift_ms: float          # shift that maximises contrast within +/- ALIGN_SEARCH_MS
    flags: list[str]


def _band_envelope_db(y: np.ndarray, sr: int) -> np.ndarray:
    """1 ms band-limited (1.5-10 kHz) energy envelope in dB."""
    Y = np.fft.rfft(y)
    fr = np.fft.rfftfreq(len(y), 1 / sr)
    lo, hi = config.ALIGN_BAND_HZ
    Y[(fr < lo) | (fr > hi)] = 0
    yb = np.fft.irfft(Y, n=len(y))
    hop = max(1, int(round(sr / 1000)))
    n = len(yb) // hop
    env = (yb[: n * hop].reshape(n, hop) ** 2).mean(axis=1)
    return 10 * np.log10(env + 1e-12)


def _contrast(env_db: np.ndarray, on_ms: np.ndarray, off_ms: np.ndarray, shift: float) -> float:
    T = env_db.size
    ins = np.zeros(T, bool); gap = np.zeros(T, bool)
    for a, b in zip(on_ms, off_ms):
        ins[max(0, int(a + shift)): max(0, int(b + shift))] = True
    for b, a in zip(off_ms[:-1], on_ms[1:]):
        gap[max(0, int(b + shift)): max(0, int(a + shift))] = True
    if ins.sum() < 3 or gap.sum() < 3:
        return float("nan")
    return float(env_db[ins].mean() - env_db[gap].mean())


def align(bout: ChipperBout, y: np.ndarray, sr_native: int, n_native: int) -> Alignment:
    """Derive audio-time onsets/offsets and QA the alignment.

    ``y`` is the audio at ``config.SR`` (used for the energy check); ``n_native`` /
    ``sr_native`` describe the file as Chipper saw it (padding is measured there).
    """
    pad = bout.width_px * bout.ms_per_px / 1000 * sr_native - n_native
    shift_ms = -(pad / 2) / sr_native * 1000
    on_ms = bout.onsets_px * bout.ms_per_px + shift_ms
    off_ms = bout.offsets_px * bout.ms_per_px + shift_ms

    env = _band_envelope_db(y, config.SR)
    chip_on = bout.onsets_px * bout.ms_per_px
    chip_off = bout.offsets_px * bout.ms_per_px
    contrast = _contrast(env, chip_on, chip_off, shift_ms)
    shifts = np.arange(-config.ALIGN_SEARCH_MS, config.ALIGN_SEARCH_MS + 1, 2)
    cs = np.array([_contrast(env, chip_on, chip_off, s) for s in shifts])
    best = float(shifts[int(np.nanargmax(cs))]) if np.isfinite(cs).any() else float("nan")

    flags = []
    lo, hi = config.PAD_SAMPLES_OK
    if not (lo <= pad <= hi):
        flags.append("length_mismatch")          # wav and gzip disagree on length -> probably different file versions
    if np.isfinite(best) and abs(best - shift_ms) > config.ALIGN_MAX_DEV_MS:
        flags.append("misaligned")
    if not np.isfinite(contrast) or contrast < config.ALIGN_MIN_CONTRAST_DB:
        flags.append("weak_contrast")
    dur_ms = len(y) / config.SR * 1000
    if on_ms[0] < -1 or off_ms[-1] > dur_ms + 1:
        flags.append("outside_audio")
    return Alignment(float(pad), float(shift_ms), on_ms, off_ms, contrast, best, flags)
