# Syllable Atlas → moved

The syllable atlas that was developed here (Sept 2026) now lives in two repositories:

- **Code (public):** https://github.com/ktsnyder/syllable-atlas — a dataset-agnostic syllable
  browser (Chipper gzips, onset/offset tables or WhisperSeg output + a reference sheet), with
  the song sparrow dataset described in `datasets/song_sparrows_full.yaml` and two small example
  datasets so it runs out of the box.
- **Data (private, lab members):** https://github.com/CreanzaLab/SongSparrow-data — the 1,804
  Chipper gzips, the reference sheet, the 2022 analysis tables and the 2,432 bout wavs (as release
  assets). Fetch with `scripts/fetch_songsparrow_data.sh` in the atlas repo.

The commit history of the original development is in this branch (`SyllableExploration`) up to
commit 300baa5.
