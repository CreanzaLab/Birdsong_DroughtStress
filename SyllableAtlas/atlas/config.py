"""All tunables for the syllable atlas in one place.

Override the two roots with environment variables:
  SOSP_ROOT      folder holding GZIPs/, Wavs/, AnalyzedData/ and the metadata CSV
  ATLAS_DATA_DIR where generated audio / images / tables go (default: SyllableAtlas/atlas_data)
"""
from __future__ import annotations
import os
from pathlib import Path

HERE = Path(__file__).resolve().parent.parent

SOSP_ROOT = Path(os.environ.get("SOSP_ROOT", "/Users/kate/Documents/Creanza_Lab/SongSparrows"))
GZIP_DIR = SOSP_ROOT / "GZIPs"
WAV_DIR = SOSP_ROOT / "Wavs"
METADATA_CSV = SOSP_ROOT / "2022-08-31_20220730_SOSP_Complex_Metadata_addedJuvSong_NewRegions.csv"
SYLL_TABLE_CSV = (SOSP_ROOT / "AnalyzedData" /
                  "2022-11-15_NoteAnalysisBySyll_RemovedTowhees_Removed181230411_PlusMetadata_InRegionEra_AddedOnlyUsedMissingRecs.csv")
BOUT_TABLE_CSV = SOSP_ROOT / "AnalyzedData" / "2022-11-16_SOSP_bout-stats_plusMetadata_RemovedUnusedBouts.csv"

DATA_DIR = Path(os.environ.get("ATLAS_DATA_DIR", HERE / "atlas_data"))
IMG_DIR = DATA_DIR / "img"
AUDIO_DIR = DATA_DIR / "audio"
CACHE_DIR = DATA_DIR / "cache"

# ---- analysis signal -------------------------------------------------------
SR = 44_100                     # every bout is resampled to this for features, audio and images
N_FFT = 512                     # ~11.6 ms window: syllables can be 20-60 ms long
HOP = 64                        # ~1.5 ms hop
F0_FMIN = 1_000.0               # song sparrow syllables live ~1.5-10 kHz
F0_FMAX = 10_000.0
PYIN_FRAME = 1024
PYIN_HOP = 128
MIN_PYIN_SAMPLES = 2048         # clips shorter than this skip pitch tracking (NaN)
ROLLOFF_PERCENT = 0.85
FEATURE_HIGHPASS_HZ = 500.0      # floor for the per-bout band-pass when Chipper's own high-pass was lower (audio + images are NOT filtered)

# ---- Chipper alignment -----------------------------------------------------
# Chipper's sonogram is wider than the audio by ~3175 samples regardless of
# sample rate (empirically 3168-3190 across 44.1/48/22.05/16 kHz files), and the
# padding is split evenly between the start and the end (empirical best shift
# -32..-36 ms at 44.1 kHz  ==  -pad/2). audio_time = chipper_time - pad/2.
PAD_SAMPLES_OK = (3000, 3400)   # outside this -> wav/gzip length mismatch flag
ALIGN_SEARCH_MS = 100           # +/- search window for the best-shift QA check
ALIGN_MAX_DEV_MS = 12.0         # |best shift - derived shift| above this -> "misaligned" flag
ALIGN_MIN_CONTRAST_DB = 6.0     # syllable-vs-gap energy contrast below this -> "weak" flag
ALIGN_BAND_HZ = (1_500.0, 10_000.0)

# ---- rendering -------------------------------------------------------------
RENDER_N_FFT = 1024
RENDER_HOP = 128                # 1 px per frame -> px_per_ms = SR / RENDER_HOP / 1000
RENDER_FMAX = 10_000.0
RENDER_DB_WINDOW = 55.0         # dB below the per-image peak that maps to white
IMG_HEIGHT = 240                # px

N_WORKERS = max(1, (os.cpu_count() or 4) - 1)
