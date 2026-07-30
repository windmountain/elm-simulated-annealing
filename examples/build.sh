#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

for src in src/*.elm; do
  name=$(basename "$src" .elm)
  grep -q '^module .* exposing (main)' "$src" || continue

  kebab=$(echo "$name" | sed -E 's/([a-z0-9])([A-Z])/\1-\2/g' | tr '[:upper:]' '[:lower:]')
  html="$kebab.html"

  echo "Building $src -> $html"
  elm make "$src" --output="$html"

  # elm make always emits `body { padding: 0; margin: 0; }` — give every
  # example page some breathing room from the browser edges instead.
  sed -i '' 's/body { padding: 0; margin: 0; }/body { padding: 0; margin: 3em; }/' "$html"
done
