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
