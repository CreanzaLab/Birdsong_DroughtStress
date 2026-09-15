"""Viterbi peak-frequency tracking and frequency modulation, ported from
02_CalculateSyllableMetrics_newFM.R (Darra Boyer; Viterbi DP peak frequency +
3-point median FM tracking, 2026-09-10).

Steps, matching the R implementation:
  1. spectrogram of the syllable with an adaptive window (wl=128 for < 1500 samples,
     else 512; 90 % overlap), amplitudes in dB relative to the syllable maximum
  2. keep only frequency bins inside Chipper's syllable bounds [lower, upper] Hz
  3. drop frames whose peak is > 20 dB below the syllable maximum
  4. Viterbi dynamic programming: per-frame min-max-normalised amplitude as reward,
     jump penalty 0.05 per kHz of frequency change between frames
  5. trim onset/offset artifacts (up to 3 frames with a > 1500 Hz jump)
  6. 3-point running median of the track
  FM (kHz/s) = mean |Δ track| / Δt, raw and median-filtered.
"""
from __future__ import annotations

import numpy as np

_NAN = float("nan")
EMPTY = {"vit_peak_freq_med_hz": _NAN, "vit_peak_freq_max_hz": _NAN, "vit_peak_freq_min_hz": _NAN,
         "vit_peak_bandwidth_hz": _NAN, "vit_fm_raw_khz_s": _NAN, "vit_fm_filtered_khz_s": _NAN}


def _spectro_db(y: np.ndarray, sr: int, wl: int, ovlp: float = 0.90):
    hop = max(1, int(round(wl * (1 - ovlp))))
    if y.size < wl:
        y = np.pad(y, (0, wl - y.size))
    n_frames = 1 + (y.size - wl) // hop
    win = np.hanning(wl)
    frames = np.stack([y[i * hop: i * hop + wl] * win for i in range(n_frames)], axis=1)   # wl x n_frames
    mag = np.abs(np.fft.rfft(frames, axis=0))
    freqs = np.fft.rfftfreq(wl, 1 / sr)
    db = 20 * np.log10(mag / (mag.max() + 1e-12) + 1e-12)
    times = (np.arange(n_frames) * hop + wl / 2) / sr
    return db, freqs, times


def viterbi_peak_track(amp: np.ndarray, freqs: np.ndarray, jump_penalty: float = 0.05) -> np.ndarray:
    n_f, n_t = amp.shape
    if n_t == 0:
        return np.array([])
    if n_t == 1:
        return np.array([freqs[int(np.argmax(amp[:, 0]))]])
    cmin, cmax = amp.min(axis=0), amp.max(axis=0)
    span = np.where(cmax > cmin, cmax - cmin, 1.0)
    norm = np.where(cmax > cmin, (amp - cmin) / span, 0.0)
    f_khz = freqs / 1000.0
    jump = jump_penalty * np.abs(f_khz[:, None] - f_khz[None, :])       # j x k : cost of moving k -> j
    cost = np.full((n_f, n_t), np.inf)
    back = np.zeros((n_f, n_t), dtype=int)
    cost[:, 0] = -norm[:, 0]
    for t in range(1, n_t):
        trans = cost[:, t - 1][None, :] + jump                              # j x k
        best_prev = np.argmin(trans, axis=1)
        c = trans[np.arange(n_f), best_prev] - norm[:, t]
        valid = norm[:, t] > 0
        cost[valid, t] = c[valid]
        back[:, t] = best_prev
    path = np.zeros(n_t, dtype=int)
    cur = int(np.argmin(cost[:, -1]))
    path[-1] = cur
    for t in range(n_t - 1, 0, -1):
        prev = back[cur, t]
        if not np.isfinite(cost[prev, t - 1]):
            prev = int(np.argmax(norm[:, t - 1]))
        cur = prev
        path[t - 1] = cur
    return freqs[path]


def trim_edge_artifacts(track: np.ndarray, times: np.ndarray, max_trim: int = 3, slope_thresh: float = 1500.0):
    n = track.size
    if n < 5:
        return track, times
    start = 0
    for i in range(min(max_trim, n - 3)):
        if abs(track[i + 1] - track[i]) > slope_thresh:
            if abs(track[i + 2] - track[i + 1]) < slope_thresh / 2:
                start = i + 1
        else:
            break
    end = n
    for i in range(n - 1, max(n - max_trim, 3) - 1, -1):
        if abs(track[i] - track[i - 1]) > slope_thresh:
            end = i
        else:
            break
    if start < end:
        return track[start:end], times[start:end]
    return track, times


def _runmed3(x: np.ndarray) -> np.ndarray:
    if x.size < 3:
        return x
    out = x.copy()
    for i in range(1, x.size - 1):
        out[i] = np.median(x[i - 1: i + 2])
    return out


def peak_metrics(y: np.ndarray, sr: int, low_hz: float | None, high_hz: float | None) -> dict[str, float]:
    """Peak-frequency track statistics + FM for one syllable clip, constrained to
    Chipper's syllable frequency bounds (Hz)."""
    n = y.size
    if n < 32:
        return dict(EMPTY)
    wl = min(128, 2 * (n // 4)) if n < 1500 else min(512, 2 * (n // 2))
    wl = max(32, wl)
    db, freqs, times = _spectro_db(y.astype(np.float64), sr, wl)
    if db.shape[1] < 2:
        return dict(EMPTY)
    lo = low_hz if (low_hz is not None and np.isfinite(low_hz) and low_hz >= 0) else 0.0
    hi = high_hz if (high_hz is not None and np.isfinite(high_hz) and high_hz > lo) else sr / 2
    bin_w = freqs[1] - freqs[0] if freqs.size > 1 else 100.0
    valid = (freqs >= lo - 0.5 * bin_w) & (freqs <= hi + 0.5 * bin_w) & (freqs > 0)
    if not valid.any():
        mid = (lo + hi) / 2
        nz = np.where(freqs > 0)[0]
        valid[nz[int(np.argmin(np.abs(freqs[nz] - mid)))]] = True
    amp, fr = db[valid], freqs[valid]
    frame_max = amp.max(axis=0)
    mask = frame_max >= amp.max() - 20.0
    if not mask.any():
        mask[:] = True
    amp_m, t_m = amp[:, mask], times[mask]
    if amp_m.shape[1] < 2:
        return dict(EMPTY)
    raw = viterbi_peak_track(amp_m, fr)
    clean, t_clean = trim_edge_artifacts(raw, t_m)
    med = _runmed3(clean) if clean.size >= 3 else clean
    dt = float(np.mean(np.diff(t_clean))) if t_clean.size > 1 else 0.0013
    if not np.isfinite(dt) or dt <= 0:
        dt = 0.0013
    fm_raw = float(np.mean(np.abs(np.diff(clean))) / dt / 1000.0) if clean.size > 1 else 0.0
    fm_med = float(np.mean(np.abs(np.diff(med))) / dt / 1000.0) if med.size > 1 else 0.0
    return {"vit_peak_freq_med_hz": float(np.median(med)), "vit_peak_freq_max_hz": float(med.max()),
            "vit_peak_freq_min_hz": float(med.min()), "vit_peak_bandwidth_hz": float(med.max() - med.min()),
            "vit_fm_raw_khz_s": fm_raw, "vit_fm_filtered_khz_s": fm_med}
