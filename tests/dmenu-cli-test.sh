#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
output=$(printf '%s\n' alpha | guile --no-auto-compile \
  -L "$project_dir/tests/cli-fixture" -L "$project_dir/src" \
  "$project_dir/scripts/dmenu" --input-wrap || test "$?" -eq 1)
test "$output" = 'input-wrap=enabled'
printf '%s\n' 'dmenu --input-wrap CLI propagation: PASS'
