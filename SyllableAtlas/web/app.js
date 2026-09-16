/* Song Sparrow Syllable Atlas — static, single-page.
   Data: atlas_data/syllables.json (columnar), bouts.json (per-bout audio/image/onsets), meta.json (field groups).
   Interaction model follows sound-library's Plots page (hover -> spectrogram + optional audio, color by anything,
   click -> pinned detail) plus the "bout path" mode: hovering a syllable draws the path through every syllable of
   its bout in the current axes and shows the whole song with the hovered syllable marked. */

const PALETTE = ['#b08527', '#2f6b6b', '#9a4f3f', '#3b6ea5', '#6b7d3a', '#7a5a8f', '#b5763b', '#4a4640',
                 '#c96f3a', '#4f8a8b', '#8c5a6f', '#5a7fb0', '#8a9a4a', '#a06a9f', '#c98a4b', '#6a665c'];
const DATA = '../atlas_data/';
const $ = (id) => document.getElementById(id);

const state = {
  x: 'duration_ms', y: 'peak_frequency_hz', z: '', color: 'region', hover: 'syllable', play: 'off', view: 'axes',
  logx: false, logy: false, expr: '', pinned: null, hovered: null,
};
let meta, cols = {}, N = 0, bouts = {}, rowsById = {};
let filteredIdx = [];        // row indices currently plotted
let traceOfRow = [];         // for tween bookkeeping: [traceIndex, pointIndex]
let nDataTraces = 0;         // path + highlight traces follow the data traces
let drawnSig = '';
const gd = $('plot');

/* ------------------------------------------------------------------ data */
async function load() {
  const [m, s, b] = await Promise.all([
    fetch(DATA + 'meta.json').then((r) => r.json()),
    fetch(DATA + 'syllables.json').then((r) => r.json()),
    fetch(DATA + 'bouts.json').then((r) => r.json()),
  ]);
  meta = m; bouts = b; N = s.rows.length;
  s.fields.forEach((f, j) => { cols[f] = s.rows.map((r) => r[j]); });
  cols.id.forEach((id, i) => { rowsById[id] = i; });
  // bout -> row indices in syllable order (for the path)
  bouts._rows = {};
  cols.bout_key.forEach((k, i) => (bouts._rows[k] ??= []).push(i));
  for (const k in bouts._rows) bouts._rows[k].sort((a, b) => cols.syll_num[a] - cols.syll_num[b]);
  pcaDefaults(); loadPcaSel();
  buildControls();
  $('status').textContent = `${N.toLocaleString()} syllables · ${meta.n_bouts.toLocaleString()} bouts · built ${meta.built}`;
  redraw();
}

function numericFields() { return meta.groups.flatMap((g) => g.columns); }
function isCategorical(f) { return meta.categorical.includes(f); }

function buildControls() {
  const opt = (v, label = v) => { const o = document.createElement('option'); o.value = v; o.textContent = label; return o; };
  const fillNumeric = (sel, allowEmpty) => {
    if (allowEmpty) sel.appendChild(opt('', '(2D)'));
    for (const g of meta.groups) {
      if (/^Original 2022/.test(g.name)) continue;          // table values stay in the detail panel / filters, not as axes
      const og = document.createElement('optgroup'); og.label = g.name;
      g.columns.forEach((c) => og.appendChild(opt(c, prettyName(c))));
      sel.appendChild(og);
    }
  };
  fillNumeric($('x')); fillNumeric($('y')); fillNumeric($('z'), true);
  for (const sel of [$('x'), $('y'), $('z')]) {
    const og = document.createElement('optgroup'); og.label = 'PCA (computed on the current filter)';
    ['pc1', 'pc2', 'pc3'].forEach((c) => og.appendChild(opt(c, c.toUpperCase())));
    sel.appendChild(og);
  }
  const cs = $('color');
  const ogc = document.createElement('optgroup'); ogc.label = 'Categories';
  meta.categorical.filter((c) => !['bout_key', 'qa_flags'].includes(c) || true).forEach((c) => ogc.appendChild(opt(c, prettyName(c))));
  cs.appendChild(ogc);
  for (const g of meta.groups) {
    const og = document.createElement('optgroup'); og.label = g.name;
    g.columns.forEach((c) => og.appendChild(opt(c, prettyName(c))));
    cs.appendChild(og);
  }
  $('x').value = state.x; $('y').value = state.y; $('color').value = state.color;
  for (const f of ['region', 'era', 'source']) {
    const sel = $('f_' + f), vals = [...new Set(cols[f])].sort();
    vals.forEach((v) => sel.appendChild(opt(v)));
    sel.size = 1; sel.addEventListener('change', redraw);
  }
  ['x', 'y', 'z', 'color'].forEach((k) => $(k).addEventListener('change', (e) => {
    state[k] = e.target.value;
    if (k !== 'color') state.view = [state.x, state.y, state.z].some((f) => /^pc\d$/.test(f || '')) ? 'pca' : 'axes';
    redraw();
  }));
  ['logx', 'logy'].forEach((k) => $(k).addEventListener('change', (e) => { state[k] = e.target.checked; redraw(); }));
  ['f_final', 'f_qa'].forEach((k) => $(k).addEventListener('change', redraw));
  $('expr').addEventListener('keydown', (e) => { if (e.key === 'Enter') { state.expr = e.target.value.trim(); redraw(); } });
  $('clear').addEventListener('click', () => {
    ['region', 'era', 'source'].forEach((f) => { for (const o of $('f_' + f).options) o.selected = false; });
    $('f_final').checked = false; $('f_qa').checked = false; $('expr').value = ''; state.expr = ''; redraw();
  });
  document.querySelectorAll('[data-hover]').forEach((b) => b.addEventListener('click', () => {
    state.hover = b.dataset.hover; document.querySelectorAll('[data-hover]').forEach((x) => x.classList.toggle('on', x === b));
    if (state.hover === 'syllable' && state.pinned == null) setPath(null);
  }));
  document.querySelectorAll('[data-play]').forEach((b) => b.addEventListener('click', () => {
    state.play = b.dataset.play; document.querySelectorAll('[data-play]').forEach((x) => x.classList.toggle('on', x === b));
    stopAudio();
  }));
  $('umapbtn').addEventListener('click', () => {
    if (!cols.umap_1) { alert('No UMAP columns in this build — run  python -m atlas.umap_map  then  python -m atlas.build --assemble-only'); return; }
    state.view = 'umap'; state.x = 'umap_1'; state.y = 'umap_2'; state.z = ''; state.logx = state.logy = false;
    syncAxisControls(); redraw();
  });
  $('pcabtn').addEventListener('click', () => {
    state.view = state.view === 'pca' ? 'axes' : 'pca';
    if (state.view === 'pca') { state.x = 'pc1'; state.y = 'pc2'; state.z = ''; state.logx = state.logy = false; }
    else { state.x = 'duration_ms'; state.y = 'peak_frequency_hz'; state.z = ''; }
    syncAxisControls(); redraw();
  });
  $('pca3d').addEventListener('click', () => { state.z = state.z === 'pc3' ? '' : 'pc3'; syncAxisControls(); redraw(); });
  $('pcafeat').addEventListener('click', () => { const pnl = $('pcapanel'); pnl.hidden = !pnl.hidden; $('pcafeat').classList.toggle('on', !pnl.hidden); if (!pnl.hidden) { renderPcaPanel(); syncPcaPanel(); } });
  $('pcareset').addEventListener('click', () => { pcaDefaults(); savePcaSel(); syncPcaPanel(); redraw(); });
  $('pcaall').addEventListener('click', () => { pcaSel.feats = new Set(pcaCandidates().flatMap((g) => g.columns)); savePcaSel(); syncPcaPanel(); redraw(); });
  $('pcanone').addEventListener('click', () => { pcaSel.feats = new Set(); savePcaSel(); syncPcaPanel(); redraw(); });
  $('dclose').addEventListener('click', unpin);
  $('dstop').addEventListener('click', stopAudio);
  $('dcanvas').addEventListener('click', (ev) => {          // click a syllable bar/region in the pinned song to switch to it
    if (state.pinned == null || !$('dcanvas').dataset.xof) return;
    const r = $('dcanvas').getBoundingClientRect(), scale = $('dcanvas').width / r.width;
    const x = (ev.clientX - r.left) * scale, g = JSON.parse($('dcanvas').dataset.xof);
    const ms = g.t0 + ((x - g.L) / g.W) * (g.t1 - g.t0);
    const b = bouts[cols.bout_key[state.pinned]], rows = bouts._rows[b.bout_key];
    let k = b.onsets_ms.findIndex((a, j) => ms >= a && ms <= b.offsets_ms[j]);
    if (k < 0) { let best = Infinity; b.onsets_ms.forEach((a, j) => { const d = Math.min(Math.abs(ms - a), Math.abs(ms - b.offsets_ms[j])); if (d < best) { best = d; k = j; } }); }
    const row = rows.find((i) => cols.syll_num[i] === k + 1);
    if (row != null && row !== state.pinned) { pin(row); if (state.play !== 'off') playRow(row, 'syllable'); }
  });
  $('dplaysyll').addEventListener('click', () => state.pinned != null && playRow(state.pinned, 'syllable'));
  $('dplaysong').addEventListener('click', () => state.pinned != null && playRow(state.pinned, 'song'));
  window.addEventListener('keydown', (e) => { if (e.key === 'Escape') { unpin(); stopAudio(); } });
}

/* ------------------------------------------------------------------ glossary */
const GLOSSARY = [
  { title: 'Reading the atlas', intro: 'Every point is one Chipper-segmented syllable. Hover a point to see its spectrogram (time left→right, 0–10 kHz bottom→top, darkness = energy; each bout image is auto-leveled to its own peak over a 55 dB window). In bout-path mode the whole song is shown, the brass bar under the spectrogram marks the hovered syllable\'s onset→offset, and the black line on the plot joins the song\'s syllables in order 1→2→3…  Click a point to pin it; Esc closes.',
    terms: [
      ['syllable', 'A Chipper onset→offset unit inside a bout. Names read "recording bout n · syll k/n".'],
      ['bout', 'One song extracted from a longer recording (the wav Chipper was run on).'],
      ['Chipper sonogram', 'The strip under each spectrogram is Chipper\'s own thresholded sonogram from the gzip (black = pixels Chipper kept as signal after its frequency filter and thresholding), cropped to the same time span and 0–10 kHz. It is the matrix the onsets/offsets and the Chipper frequency bounds were computed from, so it shows exactly what Chipper "saw".'],
      ['bout path', 'The polyline through all syllables of the hovered syllable\'s bout, in the current X/Y(/Z) axes.'],
      ['QA flags', 'Per-bout alignment checks (see below). Flagged bouts are hidden by default; untick "hide QA-flagged bouts" to show them.'],
    ] },
  { title: 'QA flags', intro: 'Per-bout checks that the wav really is the audio Chipper segmented and that the onsets/offsets land on the sound.',
    terms: [
      ['length_mismatch', 'The wav is not the audio the gzip was made from (different extraction of the same bout). All such wavs were replaced from Box copies whose length matches.'],
      ['misaligned', 'The energy-maximizing shift is more than 12 ms from the derived shift.'],
      ['weak_contrast', 'Syllables are less than 6 dB louder than the gaps (noisy recording).'],
      ['outside_audio', 'An onset or offset falls outside the wav.'],
      ['bout_hpf_hz / bout_lpf_hz', 'High-/low-pass cutoffs used for every spectral feature of the bout: Chipper\'s FrequencyFilter, except that the high-pass is never below 500 Hz (if Chipper\'s was lower, 500 Hz is used; if higher, Chipper\'s value is used). Features are computed inside this band and inside the syllable\'s onset→offset.'],
    ] },
  { title: 'Original 2022 analysis', intro: 'Values joined from AnalyzedData/2022-11-15_NoteAnalysisBySyll…csv (Snyder, Sellers & Creanza 2025 pipeline). Present only for the 1,575 bouts used in that analysis.',
    terms: [
      ['table_duration_ms', 'Syllable duration from the Chipper output (ms).'],
      ['table_upper_freq_hz / table_lower_freq_hz', 'Highest / lowest frequency with signal in Chipper\'s thresholded sonogram for the syllable.'],
      ['table_freq_mod_hz', 'Upper minus lower frequency (Chipper\'s "frequency modulation").'],
      ['table_n_notes', 'Number of notes Chipper found inside the syllable.'],
      ['cluster_2022', 'Syllable-type cluster id from the June 2022 clustering (Clust20220619.Overall); 0 / NA = unassigned.'],
      ['syllable_pattern_id', 'Within-bout syllable-type id from Chipper (repeats of the same type share an id).'],
      ['removed_as_whistle / probable_whistle', 'Flags from the 2022 note analysis.'],
    ] },
  { title: 'Chipper (from gzip)', intro: 'The same quantities recomputed directly from every gzip, so they exist for all 1,804 bouts.',
    terms: [
      ['chipper_duration_ms', '(offset − onset) × ms per pixel.'],
      ['chipper_upper_freq_hz / chipper_lower_freq_hz', 'First / last sonogram row with signal, converted with hz per pixel, exactly as the original script.'],
      ['chipper_freq_range_hz', 'Upper minus lower.'],
    ] },
  { title: 'Spectral', terms: [
      ['peak_frequency_hz', 'Bin with the most energy in the syllable\'s mean spectrum.'],
      ['mean_frequency_hz', 'Power-weighted mean frequency, averaged over frames.'],
      ['freq_lo_5pct_hz / freq_hi_95pct_hz', 'Frequencies below which 5 % / 95 % of the syllable\'s energy lies — a robust bandwidth.'],
      ['spectral_centroid', '"Center of mass" of the spectrum (Hz) — high = bright.'],
      ['spectral_bandwidth', 'Spread of energy around the centroid (Hz).'],
      ['spectral_rolloff', 'Frequency below which 85 % of the energy lies.'],
      ['spectral_flatness', '0 = pure tone … 1 = noise.'],
      ['spectral_flux_mean', 'How fast the spectrum changes frame to frame.'],
    ] },
  { title: 'Pitch', terms: [
      ['f0_median / f0_min / f0_max / f0_std', 'pyin fundamental-frequency contour statistics (1–10 kHz search range).'],
      ['pitch_goodness', 'Sound Analysis Pro "goodness of pitch": prominence of the cepstral peak; high for clean harmonic sounds.'],
    ] },
  { title: 'Entropy & modulation', terms: [
      ['spectral_entropy', 'Normalized Shannon entropy of the spectrum — low for tones, high for noise.'],
      ['temporal_entropy', 'How evenly energy is spread over the syllable\'s duration.'],
      ['wiener_entropy', 'Mean log spectral flatness (SAP tonality): ≈0 noise, large-negative pure tone.'],
      ['am_mean', 'Mean |Δ log power| per frame — amplitude modulation.'],
      ['rms_mean', 'Mean RMS amplitude (bout audio is peak-normalized, so comparable within a bout only).'],
      ['attack_time', 'Seconds from onset to the RMS peak.'],
    ] },
  { title: 'Viterbi peak track (newFM)', intro: 'Port of 02_CalculateSyllableMetrics_newFM.R: an adaptive-window spectrogram of the syllable, restricted to Chipper\'s syllable frequency bounds and to frames within 20 dB of the syllable peak; a Viterbi dynamic-programming path follows the loudest frequency with a 0.05-per-kHz jump penalty; onset/offset artifacts are trimmed and a 3-point median filter applied.',
    terms: [
      ['vit_peak_freq_med_hz', 'Median of the tracked peak-frequency contour.'],
      ['vit_peak_freq_max_hz / vit_peak_freq_min_hz', 'Extremes of the (median-filtered) contour.'],
      ['vit_peak_bandwidth_hz', 'Max − min of the contour.'],
      ['vit_fm_raw_khz_s', 'Mean |slope| of the raw contour (kHz per second).'],
      ['vit_fm_filtered_khz_s', 'Mean |slope| after the 3-point median filter — the newFM measure.'],
    ] },
  { title: 'UMAP', intro: 'A 2-D (umap_1, umap_2) and 3-D (umap3_1..3) UMAP projection of the acoustic features, like the Sound Atlas map but computed from the feature table (there are no CLAP embeddings here). The exact inputs are listed in the banner when a UMAP axis is selected and in atlas_data/umap_meta.json.',
    terms: [
      ['inputs', 'Only features that exist for every syllable: duration, Chipper upper/lower frequency, peak / mean / 5 % / 95 % frequencies, centroid, bandwidth, roll-off, flatness, flux, spectral / temporal / Wiener entropy, AM, pitch goodness, attack time, and the Viterbi median peak, bandwidth and filtered FM. Each is z-scored; duration, flux, Viterbi bandwidth and FM are log10-transformed first.'],
      ['not used', '2022-table values (missing for 230 bouts), pyin f0 (missing for short syllables), position in bout, recording metadata, latitude/longitude.'],
      ['parameters', 'umap-learn, n_neighbors 15, min_dist 0.1, euclidean, random_state 0.'],
    ] },
  { title: 'PCA', intro: 'Principal component analysis computed in the browser on whatever syllables are currently filtered, so it changes when the filters change. By default it uses the same 21 z-scored inputs as the UMAP; the features… button lets you choose any set of computed features and which of them to log10-transform first. Each input is standardized (mean 0, sd 1) over the filtered syllables, the covariance matrix is diagonalized (Jacobi rotations), and syllables are projected onto the first three components. Syllables missing any chosen value are left out (the banner says how many).',
    terms: [
      ['pc1 / pc2 / pc3', 'Scores on the first three components; the axis titles show the % of total variance each explains. Signs are arbitrary (a component may flip when the subset changes).'],
      ['feature loadings', 'Side panel: the 15 features with the largest |PC1| loading, with their PC1 (brass), PC2 (teal) and PC3 (red, when PC1–3 is on) coefficients. Large same-sign loadings move together along that component.'],
      ['features…', 'Opens the input checklist: tick features to include, tick log to log10-transform heavy-tailed ones (log10(x + 1), or shifted so the minimum is 1 for features that can be negative). reset to UMAP set restores the default. Your selection is remembered in the browser.'],
      ['PC1–3', 'Puts PC3 on the Z axis (3-D view).'],
      ['UMAP vs PCA', 'UMAP is precomputed once over all syllables and preserves local neighborhoods non-linearly; PCA is linear, recomputed on the current subset, and its axes have interpretable loadings.'],
    ] },
  { title: 'Position in bout', terms: [
      ['syll_num / n_sylls_in_bout / rel_position', 'Order in the song, song length in syllables, and (k−1)/(n−1).'],
      ['click a syllable in the pinned song', 'In the detail panel, click any syllable in the spectrogram to switch the panel, the highlighted point and the bar to that syllable.'],
      ['onset_ms / offset_ms', 'In audio time (padding removed).'],
      ['gap_before_ms / gap_after_ms', 'Silence to the neighboring syllables.'],
      ['bout_duration_ms', 'Length of the bout wav.'],
    ] },
  { title: 'Recording metadata (color / filter)', terms: [
      ['recording', 'Macaulay Library catalog number, Xeno-canto id, or self-recording id.'],
      ['source', 'ML = Macaulay Library, XC = Xeno-canto, Self = own 2017 Ithaca recordings.'],
      ['era', 'Pre / Post the 2016 western-New-York drought (plus Pre2006, During, 2020-2021).'],
      ['region', 'Drought vs Control region as redefined Aug 2022 (RegionPostAug22); NA = recorded outside both regions. region_orig is the earlier definition.'],
      ['era_region', 'The era × region combination, e.g. Pre-Drought, Post-Control.'],
      ['in_final_table', 'Whether the bout is in the 2022 bout-stats table used for the paper.'],
      ['latitude / longitude / year_num', 'Recording location and year (numeric, usable as axes).'],
    ] },
];
function renderGlossary() {
  const g = $('glossary');
  if (g.dataset.done) return;
  g.dataset.done = '1';
  const esc = (t) => t.replace(/&/g, '&amp;').replace(/</g, '&lt;');
  g.innerHTML = `<div class="wrap"><h2>Glossary</h2><p class="sub">What every axis, color and flag means — and how the syllables were cut out.</p>` +
    GLOSSARY.map((sec) => `<h3>${esc(sec.title)}</h3>${sec.intro ? `<p class="intro">${esc(sec.intro)}</p>` : ''}<dl>` +
      sec.terms.map(([n, d]) => `<div class="term"><dt>${esc(n)}</dt><dd>${esc(d)}</dd></div>`).join('') + '</dl>').join('') + '</div>';
}
function showPage(pg) {
  document.querySelectorAll('[data-page]').forEach((b) => b.classList.toggle('on', b.dataset.page === pg));
  document.body.classList.toggle('page-glossary', pg === 'glossary');
  $('plotwrap').hidden = pg !== 'atlas'; $('glossary').hidden = pg !== 'glossary';
  if (pg === 'glossary') { renderGlossary(); if (state.pinned == null) $('detail').hidden = true; }
  else { if (state.pinned != null) $('detail').hidden = false; if (gd.data) Plotly.relayout(gd, { width: gd.clientWidth, height: gd.clientHeight }); }
}
document.querySelectorAll('[data-page]').forEach((b) => b.addEventListener('click', () => showPage(b.dataset.page)));

function prettyName(c) {
  const m = /^pc(\d)$/.exec(c || '');
  if (m && pca) return `PC${m[1]} (${(pca.ev[+m[1] - 1] * 100).toFixed(0)}% of variance)`;
  return c.replace(/_hz$/, ' (Hz)').replace(/_ms$/, ' (ms)').replace(/_/g, ' ');
}

/* ------------------------------------------------------------------ filter */
function selected(id) { return [...$(id).selectedOptions].map((o) => o.value); }

function compileExpr(src) {
  if (!src) return null;
  // whitelist: identifiers that are field names, numbers, strings, comparison / boolean operators, parentheses
  const ok = /^(\s|\d+(\.\d+)?|'[^']*'|"[^"]*"|[A-Za-z_][A-Za-z0-9_.]*|==|!=|<=|>=|<|>|&&|\|\||!|\(|\)|\+|-|\*|\/)+$/;
  if (!ok.test(src)) throw new Error('unsupported characters in filter');
  const names = src.match(/[A-Za-z_][A-Za-z0-9_.]*/g) || [];
  for (const n of names) if (!(n in cols) && !['true', 'false', 'null'].includes(n)) throw new Error(`unknown field: ${n}`);
  const body = src.replace(/[A-Za-z_][A-Za-z0-9_.]*/g, (n) => (n in cols ? `c.${n}[i]` : n));
  return new Function('c', 'i', `return (${body});`); // eslint-disable-line no-new-func
}

function applyFilters() {
  const sel = { region: selected('f_region'), era: selected('f_era'), source: selected('f_source') };
  const onlyFinal = $('f_final').checked, hideQa = $('f_qa').checked;
  let fn = null, err = '';
  try { fn = compileExpr(state.expr); } catch (e) { err = e.message; }
  const out = [];
  for (let i = 0; i < N; i++) {
    let keep = true;
    for (const f in sel) if (sel[f].length && !sel[f].includes(cols[f][i])) { keep = false; break; }
    if (!keep) continue;
    if (onlyFinal && cols.in_final_table[i] !== 'True') continue;
    if (hideQa && cols.qa_ok[i] !== 'True') continue;
    if (fn) { try { if (!fn(cols, i)) continue; } catch (e) { err = e.message; break; } }
    out.push(i);
  }
  return [out, err];
}

/* ------------------------------------------------------------------ plot */
function axisVal(f, i) { const v = cols[f][i]; return v == null ? NaN : +v; }

function buildTraces(idx) {
  const c = state.color, cat = isCategorical(c), dims = state.z ? 3 : 2, t = dims === 3 ? 'scatter3d' : 'scattergl';
  const keep = idx.filter((i) => Number.isFinite(axisVal(state.x, i)) && Number.isFinite(axisVal(state.y, i)) && (dims === 2 || Number.isFinite(axisVal(state.z, i))));
  traceOfRow = new Array(N);
  const base = (pts) => ({
    type: t, mode: 'markers',
    x: pts.map((i) => axisVal(state.x, i)), y: pts.map((i) => axisVal(state.y, i)),
    ...(dims === 3 ? { z: pts.map((i) => axisVal(state.z, i)) } : {}),
    customdata: pts, hovertemplate: '<extra></extra>', hoverinfo: 'none',
  });
  let traces;
  if (cat) {
    const groups = {};
    keep.forEach((i) => (groups[String(cols[c][i])] ??= []).push(i));
    traces = Object.keys(groups).sort(smartSort).map((name, gi) => {
      const pts = groups[name];
      pts.forEach((i, pi) => (traceOfRow[i] = [gi, pi]));
      return { ...base(pts), name: `${name} (${pts.length})`, marker: { size: dims === 3 ? 3 : 5, color: PALETTE[gi % PALETTE.length], opacity: 0.8 } };
    });
  } else {
    keep.forEach((i, pi) => (traceOfRow[i] = [0, pi]));
    traces = [{ ...base(keep), name: prettyName(c),
      marker: { size: dims === 3 ? 3 : 5, color: keep.map((i) => axisVal(c, i)), colorscale: 'Cividis', showscale: true, opacity: 0.85,
                colorbar: { thickness: 10, outlinewidth: 0, title: { text: prettyName(c), side: 'right' } } } }];
  }
  nDataTraces = traces.length;
  // bout-path trace + highlight trace (kept empty until a hover)
  traces.push({ type: t, mode: 'lines+markers', name: 'bout path', showlegend: false, hoverinfo: 'skip', x: [], y: [], ...(dims === 3 ? { z: [] } : {}),
    line: { color: '#ffffff', width: 4 }, marker: { size: 9, color: '#ffffff', line: { color: '#1b1a17', width: 2 } }, text: [], textposition: 'top center', textfont: { size: 10, color: '#1b1a17' } });
  traces.push({ type: t, mode: 'markers', name: 'hovered', showlegend: false, hoverinfo: 'skip', x: [], y: [], ...(dims === 3 ? { z: [] } : {}),
    marker: { size: 13, color: 'rgba(0,0,0,0)', line: { color: '#b08527', width: 2.5 } } });
  return [traces, keep.length];
}

function smartSort(a, b) { const na = +a, nb = +b; return Number.isFinite(na) && Number.isFinite(nb) ? na - nb : a.localeCompare(b); }

function layout(dims) {
  const ax = (title, log) => ({ gridcolor: '#e0d5bd', zerolinecolor: '#ddd2bb', linecolor: '#ddd2bb', title: { text: title, font: { size: 11 } }, type: log ? 'log' : 'linear' });
  const base = { autosize: false, width: gd.clientWidth || 800, height: gd.clientHeight || 500, margin: { l: 56, r: 10, t: 8, b: 48 }, showlegend: isCategorical(state.color),
    legend: { orientation: 'h', x: 0, y: -0.12, font: { size: 11 } }, paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)',
    font: { family: 'Inter, system-ui', color: '#4a4640', size: 11 }, hovermode: 'closest', dragmode: 'pan', uirevision: 'keep' };
  return dims === 3
    ? { ...base, scene: { xaxis: ax(prettyName(state.x), state.logx), yaxis: ax(prettyName(state.y), state.logy), zaxis: ax(prettyName(state.z)), bgcolor: 'rgba(0,0,0,0)' } }
    : { ...base, xaxis: ax(prettyName(state.x), state.logx), yaxis: ax(prettyName(state.y), state.logy) };
}

let tweenRaf = 0, lastDims = 0;
function syncAxisControls() {
  $('x').value = state.x; $('y').value = state.y; $('z').value = state.z; $('logx').checked = state.logx; $('logy').checked = state.logy;
}

/* ------------------------------------------------------------------ PCA (in-browser, on the filtered subset, same inputs as UMAP) */
let pca = null;   // { n, ev: [..], loadings: [{feature, pc1, pc2, pc3}], nMissing }
const pcaSel = { feats: null, logs: null };            // Sets; null until initialised from meta
function pcaDefaults() {
  const u = meta.umap;
  pcaSel.feats = new Set(u ? u.features : numericFields().filter((f) => !/^(table_|umap|pc\d|bout_)/.test(f)));
  pcaSel.logs = new Set(u ? u.log10_transformed : []);
}
function pcaCandidates() {                             // features the user may pick from (grouped)
  return meta.groups.filter((g) => !/^(Original 2022|UMAP|PCA)/.test(g.name)).map((g) => ({ name: g.name, columns: g.columns.filter((c) => cols[c]) }));
}
function renderPcaPanel() {
  const list = $('pcalist');
  if (list.dataset.done) return;
  list.dataset.done = '1';
  for (const g of pcaCandidates()) {
    const h = document.createElement('div'); h.className = 'pg'; h.textContent = g.name; list.appendChild(h);
    for (const c of g.columns) {
      const row = document.createElement('div'); row.className = 'pr';
      row.innerHTML = `<label><input type="checkbox" data-f="${c}" ${pcaSel.feats.has(c) ? 'checked' : ''}/> ${prettyName(c)}</label>` +
                      `<label class="lg"><input type="checkbox" data-log="${c}" ${pcaSel.logs.has(c) ? 'checked' : ''}/> log</label>`;
      list.appendChild(row);
    }
  }
  list.addEventListener('change', (e) => {
    const t = e.target;
    if (t.dataset.f) { t.checked ? pcaSel.feats.add(t.dataset.f) : pcaSel.feats.delete(t.dataset.f); }
    if (t.dataset.log) { t.checked ? pcaSel.logs.add(t.dataset.log) : pcaSel.logs.delete(t.dataset.log); }
    savePcaSel(); redraw();
  });
}
function syncPcaPanel() {
  $('pcalist').querySelectorAll('input[data-f]').forEach((el) => { el.checked = pcaSel.feats.has(el.dataset.f); });
  $('pcalist').querySelectorAll('input[data-log]').forEach((el) => { el.checked = pcaSel.logs.has(el.dataset.log); });
  $('pcacount').textContent = `${pcaSel.feats.size} selected`;
}
function savePcaSel() { try { localStorage.setItem('atlas.pca', JSON.stringify({ feats: [...pcaSel.feats], logs: [...pcaSel.logs] })); } catch (e) { /* private mode */ } }
function loadPcaSel() {
  try { const v = JSON.parse(localStorage.getItem('atlas.pca')); if (v && v.feats?.length) { pcaSel.feats = new Set(v.feats.filter((f) => cols[f])); pcaSel.logs = new Set(v.logs || []); } } catch (e) { /* ignore */ }
}
function jacobiEigen(Ain) {           // symmetric eigen-decomposition (Jacobi rotations); returns {values, vectors(cols)}
  const n = Ain.length, A = Ain.map((r) => r.slice()), V = Array.from({ length: n }, (_, i) => Array.from({ length: n }, (_, j) => (i === j ? 1 : 0)));
  for (let sweep = 0; sweep < 100; sweep++) {
    let off = 0;
    for (let i = 0; i < n; i++) for (let j = i + 1; j < n; j++) off += A[i][j] * A[i][j];
    if (off < 1e-14) break;
    for (let p = 0; p < n; p++) for (let q = p + 1; q < n; q++) {
      if (Math.abs(A[p][q]) < 1e-15) continue;
      const th = 0.5 * Math.atan2(2 * A[p][q], A[q][q] - A[p][p]), c = Math.cos(th), sn = Math.sin(th);
      for (let k = 0; k < n; k++) { const akp = A[k][p], akq = A[k][q]; A[k][p] = c * akp - sn * akq; A[k][q] = sn * akp + c * akq; }
      for (let k = 0; k < n; k++) { const apk = A[p][k], aqk = A[q][k]; A[p][k] = c * apk - sn * aqk; A[q][k] = sn * apk + c * aqk; }
      for (let k = 0; k < n; k++) { const vkp = V[k][p], vkq = V[k][q]; V[k][p] = c * vkp - sn * vkq; V[k][q] = sn * vkp + c * vkq; }
    }
  }
  const order = [...Array(n).keys()].sort((a, b) => A[b][b] - A[a][a]);
  return { values: order.map((i) => A[i][i]), vectors: order.map((i) => V.map((row) => row[i])) };
}
function computePCA(idx) {
  if (!pcaSel.feats) pcaDefaults();
  const order = numericFields(), feats = [...pcaSel.feats].filter((f) => cols[f]).sort((a, b) => order.indexOf(a) - order.indexOf(b)), logs = pcaSel.logs;
  const rows = idx.filter((i) => feats.every((f) => cols[f][i] != null && Number.isFinite(+cols[f][i])));
  const m = feats.length, n = rows.length;
  if (m < 2 || n < m + 2) { pca = null; return; }
  // log10(x + c): c = 1 when the feature is non-negative (log1p-like, tames values near 0), else shifts the minimum to 1
  const offs = {};
  for (const f of feats) if (logs.has(f)) { let mn = Infinity; for (const i of rows) mn = Math.min(mn, +cols[f][i]); offs[f] = mn >= 0 ? 1 : 1 - mn; }
  const X = rows.map((i) => feats.map((f) => (logs.has(f) ? Math.log10(+cols[f][i] + offs[f]) : +cols[f][i])));
  const mean = feats.map((_, j) => X.reduce((a, r) => a + r[j], 0) / n);
  const sd = feats.map((_, j) => Math.sqrt(X.reduce((a, r) => a + (r[j] - mean[j]) ** 2, 0) / n) || 1);
  for (const r of X) for (let j = 0; j < m; j++) r[j] = (r[j] - mean[j]) / sd[j];
  const C = Array.from({ length: m }, () => new Array(m).fill(0));
  for (const r of X) for (let a = 0; a < m; a++) for (let b = a; b < m; b++) C[a][b] += r[a] * r[b];
  for (let a = 0; a < m; a++) for (let b = a; b < m; b++) { C[a][b] /= n - 1; C[b][a] = C[a][b]; }
  const { values, vectors } = jacobiEigen(C), tot = values.reduce((a, v) => a + v, 0);
  ['pc1', 'pc2', 'pc3'].forEach((c, k) => { cols[c] = new Array(N).fill(null); rows.forEach((i, r) => { cols[c][i] = X[r].reduce((a, v, j) => a + v * vectors[k][j], 0); }); });
  pca = { n, nMissing: idx.length - n, feats, logs: feats.filter((f) => logs.has(f)),
    ev: values.slice(0, 3).map((v) => v / tot), loadings: feats.map((f, j) => ({ feature: f, pc1: vectors[0][j], pc2: vectors[1][j], pc3: vectors[2][j] ?? 0 })) };
}
function drawLoadings() {
  const box = $('loadings');
  if (state.view !== 'pca') { box.hidden = true; return; }
  box.hidden = false;
  if (!pca) { Plotly.purge($('loadplot')); return; }
  const top = [...pca.loadings].sort((a, b) => Math.abs(b.pc1) - Math.abs(a.pc1)).slice(0, 15).reverse();
  const traces = [
    { type: 'bar', orientation: 'h', name: 'PC1', x: top.map((l) => l.pc1), y: top.map((l) => l.feature), marker: { color: '#b08527' } },
    { type: 'bar', orientation: 'h', name: 'PC2', x: top.map((l) => l.pc2), y: top.map((l) => l.feature), marker: { color: '#2f6b6b' } },
  ];
  if (state.z === 'pc3') traces.push({ type: 'bar', orientation: 'h', name: 'PC3', x: top.map((l) => l.pc3), y: top.map((l) => l.feature), marker: { color: '#9a4f3f' } });
  Plotly.react($('loadplot'), traces, { barmode: 'group', margin: { l: 150, r: 10, t: 4, b: 24 }, height: 26 * top.length + 60, width: 330, showlegend: true,
    legend: { font: { size: 10 }, orientation: 'h', y: 1.06 }, paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)',
    font: { family: 'Inter, system-ui', color: '#4a4640', size: 10 }, xaxis: { gridcolor: '#e0d5bd', zerolinecolor: '#ddd2bb' }, yaxis: { automargin: true } },
    { displaylogo: false, displayModeBar: false, responsive: false });
}

function updateUmapNote() {
  const n = $('umapnote'), u = meta.umap, on = [state.x, state.y, state.z].some((f) => /^umap/.test(f || ''));
  $('umapbtn').classList.toggle('on', on);
  $('pcabtn').classList.toggle('on', state.view === 'pca'); $('pca3d').hidden = state.view !== 'pca'; $('pca3d').classList.toggle('on', state.z === 'pc3');
  $('pcafeat').hidden = state.view !== 'pca'; if (state.view === 'pca' && pcaSel.feats) $('pcacount').textContent = `${pcaSel.feats.size} selected`;
  if (state.view === 'pca' && u) {
    n.hidden = false;
    if (!pca) { n.innerHTML = '<b>PCA</b> — pick at least 2 features (features… button) with enough complete syllables.'; return; }
    const ev = pca.ev.map((v) => (v * 100).toFixed(0) + '%').join(' / ');
    const isDefault = u && pca.feats.length === u.features.length && pca.feats.every((f) => u.features.includes(f)) && pca.logs.length === u.log10_transformed.length && pca.logs.every((f) => u.log10_transformed.includes(f));
    n.innerHTML = `<b>PCA</b> of the ${pca.n.toLocaleString()} currently filtered syllables${pca.nMissing ? ` (${pca.nMissing.toLocaleString()} left out for missing values)` : ''} · variance explained PC1 / PC2 / PC3: ${ev} · ` +
      `${pca.feats.length} z-scored inputs${isDefault ? ' (the UMAP set)' : ' (custom — features… button)'}${pca.logs.length ? `, log10 first: ${pca.logs.join(', ')}` : ''}. ` +
      `<b>Inputs:</b> ${pca.feats.join(', ')}. Recomputed whenever the filter or the selection changes.`;
    return;
  }
  if (!on || !u) { n.hidden = true; return; }
  n.hidden = false;
  n.innerHTML = `<b>UMAP</b> of ${u.n_syllables.toLocaleString()} syllables · ${u.features.length} acoustic features, each z-scored` +
    ` (log10 first: ${u.log10_transformed.join(', ')}) · n_neighbors ${u.params.n_neighbors}, min_dist ${u.params.min_dist}, ${u.params.metric}.` +
    ` <b>Inputs:</b> ${u.features.join(', ')}. <b>Not used:</b> ${u.excluded_on_purpose}.`;
}
function redraw() {
  let [idx, err] = applyFilters();
  if (state.view === 'pca' || [state.x, state.y, state.z].some((f) => /^pc\d$/.test(f || ''))) computePCA(idx);
  updateUmapNote(); drawLoadings();
  const dims3 = !!state.z, MAX_3D = 8000;
  let note = '';
  if (dims3 && idx.length > MAX_3D) {           // scatter3d + per-hover updates freeze past ~10k points
    const step = idx.length / MAX_3D;
    idx = Array.from({ length: MAX_3D }, (_, k) => idx[Math.floor(k * step)]);
    note = `  ·  3D shows an evenly spaced subsample of ${MAX_3D.toLocaleString()} (clear Z for all points)`;
  }
  filteredIdx = idx;
  const [traces, n] = buildTraces(idx);
  const dims = state.z ? 3 : 2;
  const sig = `${dims}|${state.color}|${state.logx}|${state.logy}|${idx.length}|${idx[0]}|${idx[idx.length - 1]}|${traces.slice(0, nDataTraces).map((t) => t.name).join('~')}`;
  $('status').textContent = `${n.toLocaleString()} of ${N.toLocaleString()} syllables` + note + (err ? `  ·  filter error: ${err}` : '');
  const cfg = { displaylogo: false, scrollZoom: true, responsive: true, modeBarButtonsToRemove: ['lasso2d', 'select2d'] };
  cancelAnimationFrame(tweenRaf);
  if (dims !== lastDims && gd.data) { Plotly.purge(gd); bound = false; pathState.rows = null; pathState.hl = null; drawPathOverlay(); }   // a stale WebGL scene otherwise survives the switch
  lastDims = dims;
  if (sig !== drawnSig || !gd.data || dims === 3) {
    drawnSig = sig;
    Plotly.react(gd, traces, layout(dims), cfg);
    bindPlotEvents();
  } else {
    // same structure, new axes: LERP the coordinates (Plotly can't transition scattergl)
    Plotly.relayout(gd, { 'xaxis.title.text': prettyName(state.x), 'yaxis.title.text': prettyName(state.y), 'xaxis.type': state.logx ? 'log' : 'linear', 'yaxis.type': state.logy ? 'log' : 'linear' });
    const starts = gd.data.slice(0, nDataTraces).map((t) => ({ x: Array.from(t.x), y: Array.from(t.y) }));
    const t0 = performance.now(), dur = 450, ease = (u) => u * u * u * (u * (u * 6 - 15) + 10);
    const step = (now) => {
      const u = Math.min(1, (now - t0) / dur), e = ease(u);
      const upd = { x: [], y: [] };
      for (let k = 0; k < nDataTraces; k++) {
        upd.x.push(starts[k].x.map((v, i) => v + (traces[k].x[i] - v) * e));
        upd.y.push(starts[k].y.map((v, i) => v + (traces[k].y[i] - v) * e));
      }
      Plotly.restyle(gd, upd, [...Array(nDataTraces).keys()]);
      if (u < 1) tweenRaf = requestAnimationFrame(step); else Plotly.relayout(gd, { 'xaxis.autorange': true, 'yaxis.autorange': true }).then(drawPathOverlay);
    };
    Plotly.relayout(gd, { 'xaxis.autorange': true, 'yaxis.autorange': true });
    tweenRaf = requestAnimationFrame(step);
  }
  if (state.pinned != null) { setPath(state.pinned); setHighlight(state.pinned); }
}

let bound = false;
function bindPlotEvents() {
  if (bound) return; bound = true;
  gd.on('plotly_hover', (e) => { const p = e.points?.[0]; if (p && p.curveNumber < nDataTraces) onHover(p.customdata); });
  gd.on('plotly_unhover', () => onUnhover());
  gd.on('plotly_click', (e) => { const p = e.points?.[0]; if (p && p.curveNumber < nDataTraces) pin(p.customdata); });
  gd.on('plotly_relayout', drawPathOverlay);
  gd.on('plotly_relayouting', drawPathOverlay);
  gd.on('plotly_afterplot', drawPathOverlay);
  document.addEventListener('mousemove', (ev) => { mouse.x = ev.clientX; mouse.y = ev.clientY; });
  gd.addEventListener('mouseleave', onUnhover);
  gd.addEventListener('mousedown', () => { dragging = true; hideOverlayOnly(); }, true);
  // Plotly emits plotly_click from its own mouseup handler using the current hover data; a restyle before that
  // (we run in the capture phase) wipes it and the click is lost. So finish the drag now, but clear the
  // highlight/path in a later task, and only if the click did not pin anything.
  window.addEventListener('mouseup', () => {
    dragging = false;
    if (!clearAfterDrag) return;
    clearAfterDrag = false;
    setTimeout(() => { if (state.pinned == null && lastHover == null) { setPath(null); setHighlight(null); } }, 0);
  }, true);
  gd.addEventListener('wheel', () => { lastWheel = performance.now(); hideOverlayOnly(); }, { passive: true, capture: true });
  const fit = () => { if (gd.data && !gd.hidden && gd.clientWidth > 0 && gd.clientHeight > 0) Plotly.relayout(gd, { width: gd.clientWidth, height: gd.clientHeight }); };
  window.addEventListener('resize', fit);
  new ResizeObserver(fit).observe(gd);   // also fires when the detail panel opens/closes
}
const mouse = { x: 0, y: 0 };
let dragging = false;   // Plotly cancels a pan if we restyle mid-drag, so hover is ignored while the button is down
let lastWheel = 0;      // ...and scroll-zoom is left alone for a moment after each wheel tick
let clearAfterDrag = false;
const interacting = () => dragging || performance.now() - lastWheel < 400;

/* ------------------------------------------------------------------ path + highlight
   In 2-D the bout path and the highlighted syllable are drawn in an SVG overlay positioned over the plot, so a hover
   never restyles Plotly (a scattergl restyle re-renders all ~27k points and a fast mouse used to pile those up
   into a freeze). The overlay is redrawn on pan/zoom/resize. In 3-D the path falls back to a Plotly trace. */
const pathState = { rows: null, hl: null };
const svgNS = 'http://www.w3.org/2000/svg';
function pixelOf(i) {
  const fl = gd._fullLayout, xa = fl.xaxis, ya = fl.yaxis;
  const x = xa._offset + xa.d2p(axisVal(state.x, i)), y = ya._offset + ya.d2p(axisVal(state.y, i));
  return Number.isFinite(x) && Number.isFinite(y) ? [x, y] : null;
}
function drawPathOverlay() {
  const svg = $('pathsvg');
  while (svg.firstChild) svg.removeChild(svg.firstChild);
  if (!gd._fullLayout || !gd._fullLayout.xaxis || state.z) return;      // 3-D handled by traces
  const fl = gd._fullLayout, xa = fl.xaxis, ya = fl.yaxis;
  // clip to the plotting area so panned-out syllables don't draw over the axes
  const defs = document.createElementNS(svgNS, 'defs'), cp = document.createElementNS(svgNS, 'clipPath'); cp.id = 'plotclip';
  const rect = document.createElementNS(svgNS, 'rect');
  rect.setAttribute('x', xa._offset); rect.setAttribute('y', ya._offset); rect.setAttribute('width', xa._length); rect.setAttribute('height', ya._length);
  cp.appendChild(rect); defs.appendChild(cp); svg.appendChild(defs);
  const g = document.createElementNS(svgNS, 'g'); g.setAttribute('clip-path', 'url(#plotclip)'); svg.appendChild(g);
  const mk = (tag, attrs, parent = g) => { const el = document.createElementNS(svgNS, tag); for (const k in attrs) el.setAttribute(k, attrs[k]); parent.appendChild(el); return el; };
  const rows = pathState.rows || [];
  const pts = rows.map((r) => [r, pixelOf(r)]).filter((p) => p[1]);
  if (pts.length > 1) {
    const d = pts.map(([, p], k) => (k ? 'L' : 'M') + p[0].toFixed(1) + ' ' + p[1].toFixed(1)).join(' ');
    mk('path', { d, fill: 'none', stroke: '#1b1a17', 'stroke-width': 4.5, 'stroke-linejoin': 'round', 'stroke-linecap': 'round', opacity: 0.9 });
    mk('path', { d, fill: 'none', stroke: '#ffffff', 'stroke-width': 2, 'stroke-linejoin': 'round', 'stroke-linecap': 'round' });
  }
  for (const [r, p] of pts) {
    const hl = r === pathState.hl, n = String(cols.syll_num[r]), rad = hl ? 12 : 10;
    mk('circle', { cx: p[0], cy: p[1], r: rad, fill: hl ? '#b08527' : '#ffffff', stroke: '#1b1a17', 'stroke-width': hl ? 2 : 1.5 });
    const t = mk('text', { x: p[0], y: p[1], 'text-anchor': 'middle', 'dominant-baseline': 'central', 'font-size': n.length > 2 ? 8 : 10, 'font-weight': 600,
      'font-family': 'Inter, system-ui, sans-serif', fill: hl ? '#ffffff' : '#1b1a17' });
    t.textContent = n;
  }
  if (pathState.hl != null && !rows.includes(pathState.hl)) {           // highlight alone (syllable hover mode)
    const p = pixelOf(pathState.hl);
    if (p) mk('circle', { cx: p[0], cy: p[1], r: 9, fill: 'none', stroke: '#b08527', 'stroke-width': 3 });
  }
}
function setPath(i) {
  if (state.z) {                                                         // 3-D: Plotly trace
    const pi = nDataTraces;
    if (i == null) { Plotly.restyle(gd, { x: [[]], y: [[]], z: [[]], text: [[]] }, [pi]); return; }
    const rows = bouts._rows[cols.bout_key[i]].filter((r) => Number.isFinite(axisVal(state.x, r)) && Number.isFinite(axisVal(state.y, r)) && Number.isFinite(axisVal(state.z, r)));
    Plotly.restyle(gd, { x: [rows.map((r) => axisVal(state.x, r))], y: [rows.map((r) => axisVal(state.y, r))], z: [rows.map((r) => axisVal(state.z, r))],
      text: [rows.map((r) => String(cols.syll_num[r]))], mode: 'lines+markers+text' }, [pi]);
    return;
  }
  pathState.rows = i == null ? null : bouts._rows[cols.bout_key[i]];
  drawPathOverlay();
}
function setHighlight(i) {
  if (state.z) return;                                                   // 3-D: the hover card is enough
  pathState.hl = i;
  drawPathOverlay();
}

/* ------------------------------------------------------------------ hover */
let lastHover = null;
function hideOverlayOnly() {          // DOM-only: safe to call mid-drag (no Plotly restyle)
  $('overlay').hidden = true; stopAudio(); lastHover = null; state.hovered = null;
  if (state.pinned == null) clearAfterDrag = true;
}
function onHover(i) {
  if (interacting() || i === lastHover) return;
  lastHover = i; state.hovered = i;
  const ov = $('overlay');
  drawSyllable($('ocanvas'), i, state.hover === 'bout' ? 'bout' : 'syllable', state.hover === 'bout' ? 560 : 260).then(() => {
    if (state.hovered !== i) return;
    $('oname').textContent = cols.name[i] + (cols.qa_flags[i] !== 'none' ? `  ⚠ ${cols.qa_flags[i]}` : '');
    ov.hidden = false;
    const w = ov.offsetWidth, h = ov.offsetHeight;
    ov.style.left = `${Math.min(mouse.x - 12, window.innerWidth - w - 8)}px`;
    ov.style.top = `${mouse.y + 14 + h > window.innerHeight ? mouse.y - h - 10 : mouse.y + 14}px`;
  });
  if (state.hover === 'bout' && state.pinned == null) setPath(i);
  setHighlight(i);
  if (state.play !== 'off') playRow(i, state.play);
}
function onUnhover() {
  if (interacting()) { hideOverlayOnly(); return; }
  lastHover = null; state.hovered = null;
  $('overlay').hidden = true;
  stopAudio();
  if (state.pinned == null) { setPath(null); setHighlight(null); } else { setHighlight(state.pinned); }
}

/* ------------------------------------------------------------------ pin / detail */
function pin(i) {
  state.pinned = i;
  const d = $('detail'); d.hidden = false;
  $('dname').textContent = cols.name[i];
  const b = bouts[cols.bout_key[i]];
  $('dqa').textContent = b.qa.flags.length ? `QA: ${b.qa.flags.join(', ')}` : `alignment ok · ${b.qa.align_contrast_db?.toFixed(1)} dB contrast`;
  drawSyllable($('dcanvas'), i, 'bout', 800, true);
  const tbl = $('dtable'); tbl.innerHTML = '';
  const row = (k, v, grp) => { const tr = tbl.insertRow(); if (grp) tr.className = 'grp'; tr.insertCell().textContent = k; tr.insertCell().textContent = v; };
  row('Recording', '', true);
  for (const k of ['recording', 'source', 'era', 'region', 'state', 'county', 'year', 'recordist', 'in_final_table', 'cluster_2022', 'syllable_pattern_id', 'removed_as_whistle', 'used_data'])
    row(prettyName(k), cols[k][i]);
  for (const g of meta.groups) {
    row(g.name, '', true);
    for (const c of g.columns) { const v = cols[c][i]; row(prettyName(c), v == null ? '—' : (Math.abs(v) >= 100 ? Math.round(v).toLocaleString() : (+v).toFixed(3))); }
  }
  setPath(i); setHighlight(i);
}
function unpin() { state.pinned = null; $('detail').hidden = true; setPath(null); setHighlight(null); }

/* ------------------------------------------------------------------ spectrogram drawing (crop from the bout PNG) */
const imgCache = new Map();
function loadImg(b, which = 'png') {
  const key = b.bout_key + '|' + which;
  if (imgCache.has(key)) return imgCache.get(key);
  const p = new Promise((res) => { const im = new Image(); im.onload = () => res(im); im.onerror = () => res(null); im.src = DATA + b[which]; });
  imgCache.set(key, p);
  if (imgCache.size > 400) imgCache.delete(imgCache.keys().next().value);
  return p;
}
async function drawSyllable(canvas, i, mode, targetW, allBars = false) {
  const b = bouts[cols.bout_key[i]];
  const [im, chip] = await Promise.all([loadImg(b, 'png'), b.png_chipper ? loadImg(b, 'png_chipper') : Promise.resolve(null)]);
  if (!im) return;
  const ppm = b.px_per_ms, on = cols.onset_ms[i], off = cols.offset_ms[i];
  const pad = mode === 'syllable' ? Math.max(15, (off - on) * 0.25) : 0;              // ms of context around a syllable
  const t0 = mode === 'syllable' ? Math.max(0, on - pad) : 0, t1 = mode === 'syllable' ? Math.min(b.duration_ms, off + pad) : b.duration_ms;
  const sx = t0 * ppm, sw = (t1 - t0) * ppm;
  // layout: audio spectrogram (H) above, Chipper's thresholded sonogram (HC) below, same time crop
  const L = 26, B = 22, H = 130, HC = chip ? 70 : 0, GAP = chip ? 6 : 0;
  const W = Math.max(60, Math.min(targetW, Math.round(sw * ((H + HC) / (im.height * (chip ? 1.55 : 1))))));
  canvas.width = W + L; canvas.height = H + GAP + HC + B + 12;
  const ctx = canvas.getContext('2d');
  ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.drawImage(im, sx, 0, sw, im.height, L, 0, W, H);
  const yC = H + GAP;                                                                   // top of the Chipper strip
  if (chip) {
    ctx.fillStyle = '#faf7ef'; ctx.fillRect(L, yC, W, HC);
    ctx.drawImage(chip, sx, 0, sw, chip.height, L, yC, W, HC);
    ctx.strokeStyle = '#ddd2bb'; ctx.lineWidth = 1; ctx.strokeRect(L + 0.5, yC + 0.5, W - 1, HC - 1);
  }
  const xOf = (ms) => L + ((ms - t0) / (t1 - t0)) * W;
  canvas.dataset.xof = JSON.stringify({ L, W, t0, t1 });   // lets the detail panel map a click back to time
  // syllable bars along the bottom: the selected one solid brass, the others faint
  const y0 = yC + HC + 3;
  if (allBars || mode === 'bout') {
    ctx.fillStyle = 'rgba(176,133,39,.3)';
    b.onsets_ms.forEach((a, k) => { if (k !== cols.syll_num[i] - 1) ctx.fillRect(xOf(a), y0, Math.max(1, xOf(b.offsets_ms[k]) - xOf(a)), 5); });
  }
  ctx.fillStyle = '#b08527'; ctx.fillRect(xOf(on), y0, Math.max(2, xOf(off) - xOf(on)), 5);
  if (mode === 'syllable') {
    ctx.strokeStyle = 'rgba(176,133,39,.55)'; ctx.setLineDash([3, 3]); ctx.beginPath();
    for (const ms of [on, off]) { ctx.moveTo(xOf(ms), 0); ctx.lineTo(xOf(ms), yC + HC); }
    ctx.stroke(); ctx.setLineDash([]);
  }
  // axes: kHz ticks (left, both panels), ms ticks (bottom)
  ctx.fillStyle = '#6b6657'; ctx.font = '9px Inter, system-ui'; ctx.textAlign = 'right';
  for (let k = 0; k <= b.fmax_hz / 1000; k += 2) ctx.fillText(k + (k === 0 ? ' kHz' : ''), L - 3, H - (k * 1000 / b.fmax_hz) * H + 3);
  if (chip) {
    for (const k of [0, 5, 10]) if (k * 1000 <= b.fmax_hz) ctx.fillText(String(k), L - 3, yC + HC - (k * 1000 / b.fmax_hz) * HC + 3);
    ctx.save(); ctx.translate(L + 3, yC + 9); ctx.textAlign = 'left'; ctx.fillStyle = 'rgba(107,102,87,.85)'; ctx.font = '8px Inter, system-ui'; ctx.fillText('Chipper sonogram', 0, 0); ctx.restore();
  }
  ctx.textAlign = 'center';
  const span = t1 - t0, pxPerMs = W / span;
  const stepMs = [5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000].find((s) => s * pxPerMs >= 48) || 2000;   // labels >= 48 px apart
  const first = Math.ceil(t0 / stepMs) * stepMs;
  for (let ms = first; ms <= t1; ms += stepMs) ctx.fillText(Math.round(ms) + (ms === first ? ' ms' : ''), xOf(ms), yC + HC + B + 6);
  if (mode === 'bout') { ctx.textAlign = 'left'; ctx.fillStyle = '#b08527'; ctx.fillText(`syll ${cols.syll_num[i]} of ${b.n_sylls}`, xOf(on), yC + HC + 20); }
}

/* ------------------------------------------------------------------ audio (Web Audio; one decoded buffer per bout, play a sub-range) */
let actx = null, current = null, wanted = null;
const bufCache = new Map();
function audioCtx() { return (actx ??= new (window.AudioContext || window.webkitAudioContext)()); }
async function bufferFor(b) {
  if (bufCache.has(b.bout_key)) return bufCache.get(b.bout_key);
  const p = fetch(DATA + b.audio).then((r) => r.arrayBuffer()).then((ab) => audioCtx().decodeAudioData(ab)).catch(() => null);
  bufCache.set(b.bout_key, p);
  if (bufCache.size > 200) bufCache.delete(bufCache.keys().next().value);
  return p;
}
function stopAudio() { wanted = null; if (current) { try { current.stop(); } catch (e) { /* */ } current.disconnect(); current = null; } }
async function playRow(i, what) {
  const b = bouts[cols.bout_key[i]], key = `${i}|${what}`;
  wanted = key;
  const c = audioCtx(); if (c.state === 'suspended') { try { await c.resume(); } catch (e) { /* needs gesture */ } }
  const buf = await bufferFor(b);
  if (!buf || wanted !== key) return;
  stopAudio(); wanted = key;
  const src = c.createBufferSource(); src.buffer = buf; src.connect(c.destination);
  if (what === 'song') src.start(0, 0, buf.duration);
  else src.start(0, Math.max(0, cols.onset_ms[i] / 1000), Math.max(0.02, (cols.offset_ms[i] - cols.onset_ms[i]) / 1000));
  src.onended = () => { if (current === src) current = null; };
  current = src;
}

function waitForPlotly(ms = 20000) {
  return new Promise((res, rej) => { const t0 = Date.now(); (function poll() { if (window.Plotly) res(); else if (Date.now() - t0 > ms) rej(new Error('plotly.js did not load (offline? put a copy at web/vendor/plotly.min.js)')); else setTimeout(poll, 50); })(); });
}
waitForPlotly().then(load).catch((e) => { $('status').textContent = 'failed to load atlas_data — run  python -m atlas.build  first (' + e.message + ')'; });
