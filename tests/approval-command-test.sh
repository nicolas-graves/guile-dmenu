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
  comment=${4-}
  printf '%s\n' "$request" |
    GUILE_AUTO_COMPILE=0 \
    GUILE_LOAD_PATH="$project_dir/src${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}" \
    XDG_RUNTIME_DIR="$test_dir" \
    WAYLAND_DISPLAY=default \
    CODEX_DMENU_MOCK_RESPONSE="$response" \
    CODEX_DMENU_MOCK_COMMENT="$comment" \
    CODEX_DMENU_TIMEOUT="${CODEX_DMENU_TIMEOUT-}" \
    "$project_dir/scripts/codex-dmenu-approval" >"$output" 2>"$error"
}

run_approval 'Allow once' "$test_dir/allow" "$test_dir/allow.err"
grep -q '"behavior":"allow"' "$test_dir/allow"

run_approval 'Deny' "$test_dir/deny" "$test_dir/deny.err"
grep -q '"behavior":"deny"' "$test_dir/deny"

run_approval 'Allow once' "$test_dir/comment" "$test_dir/comment.err" \
  'Check permissions before proceeding'
grep -q '"behavior":"allow"' "$test_dir/comment"
grep -q '"systemMessage":"Selected choice: Allow once\\nComment: Check permissions before proceeding"' \
  "$test_dir/comment"

run_approval 'Deny' "$test_dir/deny-comment" "$test_dir/deny-comment.err" \
  'Use the narrower command instead'
grep -q '"behavior":"deny"' "$test_dir/deny-comment"
grep -q '"message":"Selected choice: Deny\\nComment: Use the narrower command instead"' \
  "$test_dir/deny-comment"

run_approval 'Review in terminal' "$test_dir/review" "$test_dir/review.err"
test ! -s "$test_dir/review"

# Unknown injected responses model cancellation/graphical failure and must use
# the same fail-safe empty-output contract as Review in terminal.
run_approval 'unavailable' "$test_dir/failure" "$test_dir/failure.err"
test ! -s "$test_dir/failure"

secret='must-not-leak-from-malformed-input'
printf '{"tool_input":"%s"\n' "$secret" |
  GUILE_AUTO_COMPILE=0 \
  GUILE_LOAD_PATH="$project_dir/src${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}" \
  XDG_RUNTIME_DIR="$test_dir" \
  "$project_dir/scripts/codex-dmenu-approval" \
  >"$test_dir/malformed" 2>"$test_dir/malformed.err"
test ! -s "$test_dir/malformed"
test -s "$test_dir/malformed.err"
! grep -q "$secret" "$test_dir/malformed.err"

# Hold the exact per-display lock used by the command.  Exhausting the shared
# deadline while queued must fall back silently and must never open a prompt.
lock="$test_dir/codex-dmenu-approval-default.lock"
ready="$test_dir/lock-ready"
flock "$lock" sh -c 'touch "$1"; sleep 1' sh "$ready" &
holder=$!
while test ! -e "$ready"; do sleep 0.01; done
CODEX_DMENU_TIMEOUT=0.1 run_approval 'Allow once' \
  "$test_dir/contended" "$test_dir/contended.err"
wait "$holder"
test ! -s "$test_dir/contended"
grep -q 'lock unavailable or deadline expired' "$test_dir/contended.err"

printf '%s\n' 'approval command regression: PASS'
