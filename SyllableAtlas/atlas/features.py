"""Per-syllable acoustic features (a song-sparrow-tuned subset of sound-library's
scalar feature set: no MFCCs, no spectral-contrast bands, no harmonic HNR /
inharmonicity)."""
from __future__ import annotations

import warnings

import librosa
import numpy as np

from . import config

_EPS = 1e-10

FEATURE_DOC = {
    "duration_ms": "syllable length from Chipper onset to offset (ms)",
    "rms_mean": "mean RMS amplitude",
    "peak_frequency_hz": "frequency bin with the most energy in the mean spectrum",
    "mean_frequency_hz": "power-weighted mean frequency, averaged over frames",
    "freq_lo_5pct_hz": "frequency below which 5% of the syllable's energy lies",
    "freq_hi_95pct_hz": "frequency below which 95% of the energy lies",
    "spectral_centroid": "mean spectral centroid (Hz)",
    "spectral_bandwidth": "mean spectral bandwidth (Hz)",
    "spectral_flatness": "mean spectral flatness (0 tone .. 1 noise)",
    "spectral_rolloff": "mean 85% roll-off frequency (Hz)",
    "spectral_entropy": "normalised Shannon entropy of the spectrum, averaged over frames",
    "temporal_entropy": "normalised entropy of the RMS envelope",
    "wiener_entropy": "mean log spectral flatness (SAP tonality; 0 noise, negative = tonal)",
    "pitch_goodness": "SAP goodness of pitch: prominence of the cepstral peak",
    "f0_median": "median pyin pitch (Hz), 1-10 kHz",
    "f0_std": "std of the pyin pitch contour (Hz)",
    "f0_min": "min voiced pyin pitch (Hz)",
    "f0_max": "max voiced pyin pitch (Hz)",
    "pitch_confidence": "mean pyin voicing probability",
    "fm_mean": "frequency modulation: mean |delta centroid| per frame (Hz)",
    "am_mean": "amplitude modulation: mean |delta log power| per frame",
    "spectral_flux_mean": "mean positive spectral flux",
    "zcr_mean": "mean zero-crossing rate",
    "attack_time": "time from onset to RMS peak (s)",
}
FEATURE_NAMES = list(FEATURE_DOC)


def _spectral_entropy(mag):
    power = mag ** 2
    p = power / (power.sum(axis=0, keepdims=True) + _EPS)
    ent = -(p * np.log(p + _EPS)).sum(axis=0)
    norm = np.log(mag.shape[0] + _EPS)
    return float(np.mean(ent / norm))


def _temporal_entropy(rms):
    s = rms.sum()
    if s <= _EPS:
        return 0.0
    p = rms / s
    return float(-(p * np.log(p + _EPS)).sum() / np.log(rms.size + _EPS))


def _pitch_goodness(mag, sr):
    log_mag = np.log(mag + _EPS)
    cep = np.fft.irfft(log_mag, n=config.N_FFT, axis=0)
    q_lo = max(1, int(sr / config.F0_FMAX))
    q_hi = min(cep.shape[0] // 2, int(sr / config.F0_FMIN))
    if q_hi <= q_lo:
        return float("nan")
    band = cep[q_lo:q_hi]
    return float(np.mean(band.max(axis=0) - np.median(band, axis=0)))


def _energy_quantile_freqs(mag, freqs, qs=(0.05, 0.95)):
    P = (mag ** 2).sum(axis=1)
    c = np.cumsum(P)
    if c[-1] <= _EPS:
        return [float("nan")] * len(qs)
    c = c / c[-1]
    return [float(freqs[int(np.searchsorted(c, q))]) for q in qs]


def _pitch(y, sr):
    if y.size < config.MIN_PYIN_SAMPLES:
        return float("nan"), float("nan"), float("nan"), float("nan"), 0.0
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            f0, vflag, vprob = librosa.pyin(y, fmin=config.F0_FMIN, fmax=config.F0_FMAX, sr=sr,
                                            frame_length=config.PYIN_FRAME, hop_length=config.PYIN_HOP)
    except Exception:
        return float("nan"), float("nan"), float("nan"), float("nan"), 0.0
    voiced = np.isfinite(f0) & vflag
    if voiced.sum() == 0:
        return float("nan"), float("nan"), float("nan"), float("nan"), 0.0
    v = f0[voiced]
    return float(np.median(v)), float(np.std(v)), float(v.min()), float(v.max()), float(np.mean(vprob[voiced]))


def compute(y: np.ndarray, sr: int) -> dict[str, float]:
    """Features for one syllable clip (mono float, sr = config.SR)."""
    if y.size < config.N_FFT:
        y = np.pad(y, (0, config.N_FFT - y.size))
    mag = np.abs(librosa.stft(y, n_fft=config.N_FFT, hop_length=config.HOP))
    freqs = librosa.fft_frequencies(sr=sr, n_fft=config.N_FFT)
    rms = librosa.feature.rms(y=y, frame_length=config.N_FFT, hop_length=config.HOP)[0]
    centroid = librosa.feature.spectral_centroid(S=mag, sr=sr)[0]
    bandwidth = librosa.feature.spectral_bandwidth(S=mag, sr=sr)[0]
    flatness = librosa.feature.spectral_flatness(S=mag)[0]
    rolloff = librosa.feature.spectral_rolloff(S=mag, sr=sr, roll_percent=config.ROLLOFF_PERCENT)[0]
    zcr = librosa.feature.zero_crossing_rate(y, frame_length=config.N_FFT, hop_length=config.HOP)[0]

    P = mag ** 2
    denom = P.sum(axis=0)
    mf = ((freqs[:, None] * P).sum(axis=0) / (denom + _EPS))[denom > _EPS]
    mean_spec = mag.mean(axis=1)
    f_lo, f_hi = _energy_quantile_freqs(mag, freqs)
    f0_med, f0_std, f0_min, f0_max, conf = _pitch(y, sr)

    diff = np.diff(mag, axis=1)
    flux = np.sqrt((np.maximum(diff, 0) ** 2).sum(axis=0)).mean() if mag.shape[1] > 1 else 0.0
    log_e = np.log(P.sum(axis=0) + _EPS)
    peak = int(np.argmax(rms)) if rms.size else 0
    above = np.where(rms[: peak + 1] > 0.15 * rms.max())[0] if rms.size and rms.max() > _EPS else np.array([0])
    attack = max(0.0, (peak - int(above[0])) * config.HOP / sr) if above.size else 0.0

    return {
        "duration_ms": float(y.size / sr * 1000),
        "rms_mean": float(rms.mean()),
        "peak_frequency_hz": float(freqs[int(np.argmax(mean_spec))]),
        "mean_frequency_hz": float(mf.mean()) if mf.size else float("nan"),
        "freq_lo_5pct_hz": f_lo,
        "freq_hi_95pct_hz": f_hi,
        "spectral_centroid": float(centroid.mean()),
        "spectral_bandwidth": float(bandwidth.mean()),
        "spectral_flatness": float(flatness.mean()),
        "spectral_rolloff": float(rolloff.mean()),
        "spectral_entropy": _spectral_entropy(mag),
        "temporal_entropy": _temporal_entropy(rms),
        "wiener_entropy": float(np.mean(np.log(flatness + _EPS))),
        "pitch_goodness": _pitch_goodness(mag, sr),
        "f0_median": f0_med, "f0_std": f0_std, "f0_min": f0_min, "f0_max": f0_max,
        "pitch_confidence": conf,
        "fm_mean": float(np.mean(np.abs(np.diff(centroid)))) if centroid.size > 1 else float("nan"),
        "am_mean": float(np.mean(np.abs(np.diff(log_e)))) if log_e.size > 1 else float("nan"),
        "spectral_flux_mean": float(flux),
        "zcr_mean": float(zcr.mean()),
        "attack_time": float(attack),
    }
