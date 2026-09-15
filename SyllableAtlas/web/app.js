/* Song Sparrow Syllable Atlas — static, single-page.
   Data: atlas_data/syllables.json (columnar), bouts.json (per-bout audio/image/onsets), meta.json (field groups).
   Interaction model follows sound-library's Plots page (hover -> spectrogram + optional audio, colour by anything,
   click -> pinned detail) plus the "bout path" mode: hovering a syllable draws the path through every syllable of
   its bout in the current axes and shows the whole song with the hovered syllable marked. */

const PALETTE = ['#b08527', '#2f6b6b', '#9a4f3f', '#3b6ea5', '#6b7d3a', '#7a5a8f', '#b5763b', '#4a4640',
                 '#c96f3a', '#4f8a8b', '#8c5a6f', '#5a7fb0', '#8a9a4a', '#a06a9f', '#c98a4b', '#6a665c'];
const DATA = '../atlas_data/';
const $ = (id) => document.getElementById(id);

const state = {
  x: 'duration_ms', y: 'peak_frequency_hz', z: '', color: 'region', hover: 'syllable', play: 'off',
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
      const og = document.createElement('optgroup'); og.label = g.name;
      g.columns.forEach((c) => og.appendChild(opt(c, prettyName(c))));
      sel.appendChild(og);
    }
  };
  fillNumeric($('x')); fillNumeric($('y')); fillNumeric($('z'), true);
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
  for (const f of ['region', 'era', 'source', 'state']) {
    const sel = $('f_' + f), vals = [...new Set(cols[f])].sort();
    vals.forEach((v) => sel.appendChild(opt(v)));
    sel.size = 1; sel.addEventListener('change', redraw);
  }
  ['x', 'y', 'z', 'color'].forEach((k) => $(k).addEventListener('change', (e) => { state[k] = e.target.value; redraw(); }));
  ['logx', 'logy'].forEach((k) => $(k).addEventListener('change', (e) => { state[k] = e.target.checked; redraw(); }));
  ['f_final', 'f_qa'].forEach((k) => $(k).addEventListener('change', redraw));
  $('expr').addEventListener('keydown', (e) => { if (e.key === 'Enter') { state.expr = e.target.value.trim(); redraw(); } });
  $('clear').addEventListener('click', () => {
    ['region', 'era', 'source', 'state'].forEach((f) => { for (const o of $('f_' + f).options) o.selected = false; });
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
  $('dclose').addEventListener('click', unpin);
  $('dstop').addEventListener('click', stopAudio);
  $('dplaysyll').addEventListener('click', () => state.pinned != null && playRow(state.pinned, 'syllable'));
  $('dplaysong').addEventListener('click', () => state.pinned != null && playRow(state.pinned, 'song'));
  window.addEventListener('keydown', (e) => { if (e.key === 'Escape') { unpin(); stopAudio(); } });
}

function prettyName(c) {
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
  const sel = { region: selected('f_region'), era: selected('f_era'), source: selected('f_source'), state: selected('f_state') };
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
    line: { color: '#1b1a17', width: 1.5 }, marker: { size: 7, color: '#ffffff', line: { color: '#1b1a17', width: 1.5 } }, text: [], textposition: 'top center', textfont: { size: 9 } });
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

let tweenRaf = 0;
function redraw() {
  const [idx, err] = applyFilters();
  filteredIdx = idx;
  const [traces, n] = buildTraces(idx);
  const dims = state.z ? 3 : 2;
  const sig = `${dims}|${state.color}|${state.logx}|${state.logy}|${idx.length}|${idx[0]}|${idx[idx.length - 1]}|${traces.slice(0, nDataTraces).map((t) => t.name).join('~')}`;
  $('status').textContent = `${n.toLocaleString()} of ${N.toLocaleString()} syllables` + (err ? `  ·  filter error: ${err}` : '');
  const cfg = { displaylogo: false, scrollZoom: true, responsive: true, modeBarButtonsToRemove: ['lasso2d', 'select2d'] };
  cancelAnimationFrame(tweenRaf);
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
      if (u < 1) tweenRaf = requestAnimationFrame(step); else Plotly.relayout(gd, { 'xaxis.autorange': true, 'yaxis.autorange': true });
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
  document.addEventListener('mousemove', (ev) => { mouse.x = ev.clientX; mouse.y = ev.clientY; });
  gd.addEventListener('mouseleave', onUnhover);
  const fit = () => gd.data && Plotly.relayout(gd, { width: gd.clientWidth, height: gd.clientHeight });
  window.addEventListener('resize', fit);
  new ResizeObserver(fit).observe(gd);   // also fires when the detail panel opens/closes
}
const mouse = { x: 0, y: 0 };

/* ------------------------------------------------------------------ path + highlight */
function setPath(i) {
  const pi = nDataTraces, dims = state.z ? 3 : 2;
  if (i == null) { Plotly.restyle(gd, { x: [[]], y: [[]], ...(dims === 3 ? { z: [[]] } : {}), text: [[]] }, [pi]); return; }
  const rows = bouts._rows[cols.bout_key[i]].filter((r) => Number.isFinite(axisVal(state.x, r)) && Number.isFinite(axisVal(state.y, r)));
  Plotly.restyle(gd, {
    x: [rows.map((r) => axisVal(state.x, r))], y: [rows.map((r) => axisVal(state.y, r))],
    ...(dims === 3 ? { z: [rows.map((r) => axisVal(state.z, r))] } : {}),
    text: [rows.map((r) => String(cols.syll_num[r]))], mode: rows.length > 40 ? 'lines+markers' : 'lines+markers+text',
  }, [pi]);
}
function setHighlight(i) {
  const hi = nDataTraces + 1, dims = state.z ? 3 : 2;
  if (i == null) { Plotly.restyle(gd, { x: [[]], y: [[]], ...(dims === 3 ? { z: [[]] } : {}) }, [hi]); return; }
  Plotly.restyle(gd, { x: [[axisVal(state.x, i)]], y: [[axisVal(state.y, i)]], ...(dims === 3 ? { z: [[axisVal(state.z, i)]] } : {}) }, [hi]);
}

/* ------------------------------------------------------------------ hover */
let lastHover = null;
function onHover(i) {
  if (i === lastHover) return;
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
function loadImg(b) {
  if (imgCache.has(b.bout_key)) return imgCache.get(b.bout_key);
  const p = new Promise((res, rej) => { const im = new Image(); im.onload = () => res(im); im.onerror = rej; im.src = DATA + b.png; });
  imgCache.set(b.bout_key, p); return p;
}
async function drawSyllable(canvas, i, mode, targetW, allBars = false) {
  const b = bouts[cols.bout_key[i]], im = await loadImg(b);
  const ppm = b.px_per_ms, on = cols.onset_ms[i], off = cols.offset_ms[i];
  const pad = mode === 'syllable' ? Math.max(15, (off - on) * 0.25) : 0;              // ms of context around a syllable
  const t0 = mode === 'syllable' ? Math.max(0, on - pad) : 0, t1 = mode === 'syllable' ? Math.min(b.duration_ms, off + pad) : b.duration_ms;
  const sx = t0 * ppm, sw = (t1 - t0) * ppm;
  const L = 26, B = 22, H = 150, W = Math.max(60, Math.min(targetW, Math.round(sw * (H / im.height))));
  canvas.width = W + L; canvas.height = H + B + 12;
  const ctx = canvas.getContext('2d');
  ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.drawImage(im, sx, 0, sw, im.height, L, 0, W, H);
  const xOf = (ms) => L + ((ms - t0) / (t1 - t0)) * W;
  // syllable bars along the bottom: the selected one solid brass, the others faint
  const y0 = H + 3;
  if (allBars || mode === 'bout') {
    ctx.fillStyle = 'rgba(176,133,39,.3)';
    b.onsets_ms.forEach((a, k) => { if (k !== cols.syll_num[i] - 1) ctx.fillRect(xOf(a), y0, Math.max(1, xOf(b.offsets_ms[k]) - xOf(a)), 5); });
  }
  ctx.fillStyle = '#b08527'; ctx.fillRect(xOf(on), y0, Math.max(2, xOf(off) - xOf(on)), 5);
  if (mode === 'syllable') { ctx.strokeStyle = 'rgba(176,133,39,.55)'; ctx.setLineDash([3, 3]); ctx.beginPath(); ctx.moveTo(xOf(on), 0); ctx.lineTo(xOf(on), H); ctx.moveTo(xOf(off), 0); ctx.lineTo(xOf(off), H); ctx.stroke(); ctx.setLineDash([]); }
  // axes: kHz ticks (left), ms ticks (bottom)
  ctx.fillStyle = '#6b6657'; ctx.font = '9px Inter, system-ui'; ctx.textAlign = 'right';
  for (let k = 0; k <= b.fmax_hz / 1000; k += 2) ctx.fillText(k + (k === 0 ? ' kHz' : ''), L - 3, H - (k * 1000 / b.fmax_hz) * H + 3);
  ctx.textAlign = 'center';
  const span = t1 - t0, pxPerMs = W / span;
  const stepMs = [5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000].find((s) => s * pxPerMs >= 48) || 2000;   // labels >= 48 px apart
  const first = Math.ceil(t0 / stepMs) * stepMs;
  for (let ms = first; ms <= t1; ms += stepMs) ctx.fillText(Math.round(ms) + (ms === first ? ' ms' : ''), xOf(ms), H + B + 6);
  if (mode === 'bout') { ctx.textAlign = 'left'; ctx.fillStyle = '#b08527'; ctx.fillText(`syll ${cols.syll_num[i]} of ${b.n_sylls}`, xOf(on), H + 20); }
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
