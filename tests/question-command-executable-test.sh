#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_dir=$(mktemp -d)
trap 'rm -rf -- "$test_dir"' EXIT HUP INT TERM

run_command() {
  GUILE_AUTO_COMPILE=0 \
  GUILE_LOAD_PATH="$project_dir/src${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}" \
  XDG_RUNTIME_DIR="$test_dir" \
  WAYLAND_DISPLAY=guile-dmenu-test-does-not-exist \
  "$project_dir/scripts/guile-dmenu-questions" "$@"
}

secret='must-not-leak-from-question-input'
printf '{"questions":"%s"\n' "$secret" |
  run_command >"$test_dir/malformed" 2>"$test_dir/malformed.err" || status=$?
test "${status:-0}" -eq 2
test ! -s "$test_dir/malformed"
test -s "$test_dir/malformed.err"
! grep -q "$secret" "$test_dir/malformed.err"

request='{"questions":[{"id":"choice","prompt":"Sensitive prompt","options":[{"id":"a","label":"A"},{"id":"b","label":"B"}]}]}'
printf '%s\n' "$request" |
  run_command >"$test_dir/failure" 2>"$test_dir/failure.err"
grep -q '"status":"graphical-failure"' "$test_dir/failure"
grep -q '"answers":false' "$test_dir/failure"
! grep -q 'Sensitive prompt' "$test_dir/failure.err"

printf '%s\n' "$request" |
  run_command unexpected >"$test_dir/argv" 2>"$test_dir/argv.err" || argv_status=$?
test "${argv_status:-0}" -eq 2
test ! -s "$test_dir/argv"
grep -q 'requests are accepted on stdin only' "$test_dir/argv.err"

printf '%s\n' 'question command executable regression: PASS'
