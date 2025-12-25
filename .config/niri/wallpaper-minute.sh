#!/usr/bin/env bash
set -euo pipefail

WALLDIR="$HOME/.config/niri/wallpaper"
stamp_dir="${XDG_STATE_HOME:-$HOME/.local/state}/wallpaper"
stamp_file="$stamp_dir/last-minute"

minute_full="$(date +%Y-%m-%d_%H:%M)"
prefix="$(date +%H:%M)"

mkdir -p "$stamp_dir"

[[ -f "$stamp_file" && "$(cat "$stamp_file")" == "$minute_full" ]] && exit 0

shopt -s nullglob
files=("$WALLDIR/$prefix".*)
shopt -u nullglob

[[ ${#files[@]} -eq 0 ]] && exit 0

pgrep -x swww-daemon >/dev/null || swww-daemon >/dev/null 2>&1 &

swww img "${files[0]}" --transition-type any --transition-duration 1

printf '%s' "$minute_full" > "$stamp_file"
