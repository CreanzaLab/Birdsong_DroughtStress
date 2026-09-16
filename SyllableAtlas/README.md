# Song Sparrow Syllable Atlas

A browsable map of every Chipper-segmented song sparrow syllable in the drought-stress
dataset, modelled on the [sound-library](https://github.com/ktsnyder/sound-library)
Sound Atlas "Plots" page: pick any two (or three) features for the axes, colour the
points by recording, drought region, era, syllable cluster or any numeric feature,
hover a point to see and hear it, click to pin its details.

New here: **bout path** mode. Hovering a syllable draws the path through all the
syllables of its song (1→2→3…) in the current axes, shows the *whole* song's
spectrogram with a bar under the hovered syllable, and can play the whole song
instead of the single syllable.

## Run it

```bash
cd SyllableAtlas
python3.11 -m venv .venv && .venv/bin/pip install -r requirements.txt   # once
.venv/bin/python -m atlas.build        # ~20 min for 1 804 bouts on a laptop; resumable
python serve.py                        # http://127.0.0.1:8765/web/
```

`atlas.build` expects the SongSparrows data folder at
`/Users/kate/Documents/Creanza_Lab/SongSparrows` (override with `SOSP_ROOT=…`):
`GZIPs/`, `Wavs/`, `AnalyzedData/`, and the recording-metadata CSV. Generated
outputs go to `SyllableAtlas/atlas_data/` (override with `ATLAS_DATA_DIR=…`); it is
git-ignored (~1 GB: one wav + one PNG per bout, plus the tables).

The web app is plain HTML/JS — no build step. plotly.js is loaded from the CDN unless
you drop a copy at `web/vendor/plotly.min.js`
(`curl -o web/vendor/plotly.min.js https://cdn.plot.ly/plotly-2.35.2.min.js`) for
offline use.

## What the build does

For each `SegSyllsOutput_<rec>_bout<n>.gzip` with a matching wav:

1. **Name matching** — wav, gzip and table names are normalised to one key
   (`SegSyllsOutput_` prefix, `_44k` suffix, `Melospiza-melodia-`/`-` prefixes,
   spaces, `170702_0058S12`-style underscores; see `atlas/chipper.py::norm_key`).
2. **Alignment** — Chipper's onsets/offsets are sonogram pixel columns. Its
   sonogram is ~3 175 samples wider than the audio (STFT padding) with the padding
   split evenly at both ends, so `audio_ms = px × ms_per_px − pad/2`, where
   `pad = width × ms_per_px × sr − n_samples` is measured per file. This was
   verified empirically: the syllable-vs-gap energy contrast peaks at exactly that
   shift (−32…−36 ms at 44.1 kHz) across a random sample of bouts, and holds for the
   48 kHz, 22.05 kHz and 16 kHz files, whose padding is the same number of *samples*.
3. **QA** — every bout gets `align_contrast_db` (mean band-limited dB inside
   syllables minus inside the gaps), `best_shift_ms` (the shift that maximises that
   contrast within ±100 ms) and flags: `length_mismatch` (wav and gzip disagree on
   length → probably a different extraction of the same bout; the folders once had
   two wavs with the same name), `misaligned` (best shift is >12 ms from the derived
   shift), `weak_contrast` (<6 dB), `outside_audio`. See `atlas_data/qa_report.md`
   and `qa_bouts.csv`. The web app hides flagged bouts by default (toggle in the
   filter bar) and marks them ⚠ on hover.
4. **Bounding box** — spectral features are computed strictly inside Chipper's box:
   the syllable's onset→offset in time, and the bout's `FrequencyFilter` (high-pass /
   low-pass rows × Hz-per-pixel, rows counted from 0 Hz) in frequency. **The high-pass
   is never below 500 Hz**: if Chipper's cutoff was lower, 500 Hz is used; if higher,
   Chipper's value is used. The clip is band-pass filtered to that band and STFT bins
   outside it are zeroed.
   The Viterbi peak track is further restricted to the *syllable's* Chipper
   lower/upper frequency bounds, as in the R script. Saved audio and images are
   unfiltered.
5. **Assets** — the bout is resampled to 44.1 kHz and written as a 16-bit wav; a
   greyscale spectrogram PNG (1 px per 128-sample frame, 0–10 kHz, no margins) is
   rendered once per bout. The browser crops syllables out of the bout image and
   plays sub-ranges of the bout audio, so there are no per-syllable files.
6. **Features** per syllable (`atlas/features.py`, `atlas/viterbi.py`; MFCCs, spectral-contrast bands
   and the harmonic HNR/inharmonicity measures are deliberately excluded):
   - *Original 2022 analysis* (joined from `AnalyzedData/2022-11-15_NoteAnalysisBySyll…csv`):
     `table_duration_ms`, `table_freq_mod_hz`, `table_upper_freq_hz`,
     `table_lower_freq_hz`, `table_n_notes`, plus the categorical `cluster_2022`
     (Clust20220619.Overall), `syllable_pattern_id`, `removed_as_whistle`.
   - *Chipper (from gzip)*: duration and upper/lower frequency recomputed from the
     thresholded sonogram exactly as the original script did (`chipper_*`); these
     exist for all 1 804 bouts, the table values only for the 1 575 analysed ones.
   - *Viterbi peak track (newFM)*: a Python port of
     `02_CalculateSyllableMetrics_newFM.R` — adaptive-window spectrogram, −20 dB frame
     mask, Viterbi dynamic-programming peak-frequency path (0.05/kHz jump penalty),
     edge-artifact trimming, 3-point median: `vit_peak_freq_med/max/min_hz`,
     `vit_peak_bandwidth_hz`, `vit_fm_raw_khz_s`, `vit_fm_filtered_khz_s`.
   - *Spectral*: peak / mean frequency, 5 % and 95 % energy frequencies, centroid,
     bandwidth, roll-off, flatness, flux.
   - *Pitch*: pyin f0 median/min/max/std (1–10 kHz), SAP goodness of pitch.
   - *Entropy & modulation*: spectral entropy, temporal entropy, Wiener entropy,
     AM, RMS, attack time.
   - *UMAP*: `umap_1`, `umap_2` (and `umap3_1..3`) from `atlas/umap_map.py`, run
     automatically at the end of `atlas.build` (or `python -m atlas.umap_map`). Inputs
     are only features that exist for every syllable — duration, Chipper upper/lower
     frequency, the spectral group, the entropy group, AM, pitch goodness, attack time,
     and the Viterbi median peak / bandwidth / filtered FM — each z-scored (duration,
     flux, Viterbi bandwidth and FM log10-transformed first); n_neighbors 15,
     min_dist 0.1. NOT used: 2022-table values, pyin f0, position in bout, metadata,
     lat/lon. The exact list is written to `atlas_data/umap_meta.json` and shown in a
     banner in the app whenever a UMAP axis is selected.
   - *Position in bout*: syllable number, syllables in bout, relative position,
     onset/offset (audio ms), gaps before/after, bout duration.
   - *Location & time*: latitude, longitude, year.
   - Categorical: recording, bout, source (ML/XC/Self), era, region
     (`RegionPostAug22`), **era_region** (Pre-Drought, Post-Control, …), original
     region, state, county, year, recordist, in-final-table, QA flags.

Outputs: `syllables.csv` (one row per syllable — also handy in R), `syllables.json`
and `bouts.json` (what the web app loads), `meta.json` (field groups + docs).

## Using the atlas

- **Atlas / Glossary** tabs: the glossary explains every axis, colour, flag and how
  the boundaries were derived.
- **Axes**: X / Y / optional Z (3-D) from any computed numeric feature; log toggles.
  The "Original 2022 analysis" table values are not offered as axes (they exist for
  only 1,575 bouts) but remain in the detail panel and in filter expressions. 3-D shows
  an evenly spaced subsample of 8 000 points (scatter3d cannot re-render tens of
  thousands of points on every hover).
- **Color**: any categorical field (grouped legend, click to isolate) or numeric
  feature (continuous scale).
- **UMAP view** button: sets X/Y to the UMAP projection and shows the input banner.
- **PCA view** button (like the Sound Atlas PCA page): principal components of the
  same 21 z-scored inputs as the UMAP, computed in the browser on the currently
  filtered syllables and recomputed when the filter changes. Axis titles carry the
  variance explained, a side panel shows the 15 largest PC1 loadings (PC1/PC2/PC3
  bars), and "PC1–3" puts PC3 on the Z axis. `pc1`–`pc3` are also selectable directly
  in the axis dropdowns. **features…** opens a checklist to choose which computed
  features enter the PCA and which are log10-transformed first; the banner always
  states the inputs actually used, and the choice is remembered in the browser.
- **Filters**: region / era / source multi-selects, "only bouts in the 2022
  analysis", "hide QA-flagged bouts", and a free expression such as
  `duration_ms > 100 && cluster_2022 == '1978'`.
- **Hover** → `syllable`: the syllable's spectrogram (with a little context, dashed
  onset/offset). `bout path`: the whole song with the hovered syllable's bar
  highlighted, and the path through the song's syllables drawn on the plot.
- **Play** → `off` / `syllable` / `whole song` on hover.
- **Click** a point to pin it: detail panel with the full song, all syllable bars,
  play buttons and every feature value; the path stays until you close it (Esc).
  Click any syllable in the pinned song's spectrogram to switch the panel, the
  highlighted point and the dark bar to that syllable.

## Layout

```
SyllableAtlas/
  atlas/config.py    every tunable (paths, FFT sizes, alignment thresholds, render params)
  atlas/chipper.py   gzip reader, name normalisation, onset alignment + QA
  atlas/features.py  per-syllable features
  atlas/viterbi.py   Viterbi peak-frequency track + newFM (port of 02_CalculateSyllableMetrics_newFM.R)
  atlas/umap_map.py  UMAP projection of the acoustic features (inputs recorded in atlas_data/umap_meta.json)
  atlas/render.py    bout spectrogram PNG
  atlas/build.py     the pipeline (multiprocess, resumable) + table assembly + QA report
  serve.py           tiny static server
  web/               index.html, app.js, style.css (plotly.js from CDN or web/vendor/)
  atlas_data/        generated (git-ignored)
```
