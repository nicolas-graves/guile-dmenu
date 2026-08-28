#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_dir=$(mktemp -d)
trap 'rm -rf -- "$test_dir"' EXIT HUP INT TERM

request='{"tool_name":"shell","tool_input":{"command":"true"}}'

run_approval() {
  response=$1
  output=$2
  error=$3
  printf '%s\n' "$request" |
    GUILE_AUTO_COMPILE=0 \
    GUILE_LOAD_PATH="$project_dir/src${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}" \
    XDG_RUNTIME_DIR="$test_dir" \
    CODEX_DMENU_MOCK_RESPONSE="$response" \
    "$project_dir/scripts/codex-dmenu-approval" >"$output" 2>"$error"
}

run_approval 'Allow once' "$test_dir/allow" "$test_dir/allow.err"
grep -q '"behavior":"allow"' "$test_dir/allow"

run_approval 'Deny' "$test_dir/deny" "$test_dir/deny.err"
grep -q '"behavior":"deny"' "$test_dir/deny"

run_approval 'Review in terminal' "$test_dir/review" "$test_dir/review.err"
test ! -s "$test_dir/review"

# Unknown injected responses model cancellation/graphical failure and must use
# the same fail-safe empty-output contract as Review in terminal.
run_approval 'unavailable' "$test_dir/failure" "$test_dir/failure.err"
test ! -s "$test_dir/failure"

printf '%s\n' 'approval command regression: PASS'
