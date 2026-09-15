#!/usr/bin/env python3
"""Serve the syllable atlas locally:  python serve.py  [--port 8765] [--no-browser]

Static files only (the atlas is pre-rendered), so any HTTP server works; this one
just sets the root, opens the browser, and warns if the atlas hasn't been built.
plotly.js is loaded from web/vendor/plotly.min.js if present, else from the CDN.
"""
import argparse
import http.server
import os
import sys
import webbrowser
from pathlib import Path

HERE = Path(__file__).resolve().parent
DATA = Path(os.environ.get("ATLAS_DATA_DIR", HERE / "atlas_data"))


class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *a, **k):
        super().__init__(*a, directory=str(HERE), **k)

    def translate_path(self, path):  # expose ATLAS_DATA_DIR at /atlas_data/ even when it lives elsewhere
        if path.startswith("/atlas_data/"):
            return str(DATA / path[len("/atlas_data/"):].split("?")[0])
        return super().translate_path(path)

    def end_headers(self):
        self.send_header("Cache-Control", "no-cache")
        super().end_headers()

    def log_message(self, *a):
        pass


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8765)
    ap.add_argument("--no-browser", action="store_true")
    args = ap.parse_args()
    if not (DATA / "syllables.json").exists():
        print(f"!! {DATA/'syllables.json'} not found — build the atlas first:  python -m atlas.build", file=sys.stderr)
    url = f"http://127.0.0.1:{args.port}/web/"
    print(f"Syllable atlas: {url}   (Ctrl+C to stop)")
    if not args.no_browser:
        webbrowser.open(url)
    http.server.ThreadingHTTPServer(("127.0.0.1", args.port), Handler).serve_forever()


if __name__ == "__main__":
    main()
