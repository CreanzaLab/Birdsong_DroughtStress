"""Bout spectrogram images. One greyscale PNG per bout with NO axes or margins:
1 pixel column per STFT frame, so the web app can map time <-> x exactly
(px_per_ms = SR / RENDER_HOP / 1000) and crop any syllable out of it."""
from __future__ import annotations

from pathlib import Path

import librosa
import numpy as np
from PIL import Image

from . import config


def px_per_ms() -> float:
    return config.SR / config.RENDER_HOP / 1000.0


def render_bout(y: np.ndarray, sr: int, path: Path | str) -> tuple[int, int]:
    S = np.abs(librosa.stft(y, n_fft=config.RENDER_N_FFT, hop_length=config.RENDER_HOP))
    freqs = librosa.fft_frequencies(sr=sr, n_fft=config.RENDER_N_FFT)
    S = S[freqs <= config.RENDER_FMAX]
    db = librosa.amplitude_to_db(S, ref=np.max, top_db=config.RENDER_DB_WINDOW)   # [-W, 0]
    img = 1.0 - (db + config.RENDER_DB_WINDOW) / config.RENDER_DB_WINDOW           # loud -> 0 (black), silent -> 1 (white)
    img = (np.clip(img, 0, 1) * 255).astype(np.uint8)[::-1]                        # row 0 = fmax
    im = Image.fromarray(img, mode="L").resize((img.shape[1], config.IMG_HEIGHT), Image.BILINEAR)
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    im.save(path, optimize=True)
    return im.size


def render_chipper(sono: np.ndarray, ms_per_px: float, hz_per_px: float, shift_ms: float, dur_ms: float,
                   out_width: int, path: Path | str) -> tuple[int, int]:
    """Chipper's thresholded sonogram (rows: 0 = highest frequency; 1 = signal kept) rendered to
    the SAME pixel geometry as the audio spectrogram: columns cropped to the audio's time span
    (Chipper's padding removed via ``shift_ms``), rows cropped to 0..RENDER_FMAX, resized to
    ``out_width`` x IMG_HEIGHT. Signal is black on white."""
    n_rows, n_cols = sono.shape
    c0 = int(round(-shift_ms / ms_per_px))                       # shift_ms is negative: audio starts pad/2 into the sonogram
    c1 = int(round((dur_ms - shift_ms) / ms_per_px))
    c0, c1 = max(0, c0), min(n_cols, max(c0 + 1, c1))
    r0 = max(0, n_rows - int(np.ceil(config.RENDER_FMAX / hz_per_px)))
    crop = sono[r0:, c0:c1]
    img = ((1 - crop.astype(np.float32)) * 255).astype(np.uint8)
    im = Image.fromarray(img, mode="L").resize((max(1, out_width), config.IMG_HEIGHT), Image.BILINEAR)
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    im.save(path, optimize=True)
    return im.size
