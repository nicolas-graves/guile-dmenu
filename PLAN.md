# Define Inspiration Boundaries and Feature Roadmap

## Summary

Replace the stale implementation-oriented plan with a roadmap comparing
guile-dmenu against four inspirations: bemenu, Emacs `completing-read`,
Codex-like questions, and Guile readline.

Keep dmenu itself as the project's base command contract—not a fifth comparison
grid—and identify Wayland as the technical foundation governing display and
input capabilities. Update the README to distinguish:

- supported now;
- wanted backlog;
- intentionally unsupported behavior;
- inspiration-only functionality.

This pass changes documentation and policy only. It introduces no runtime or
public API changes.

## Documentation Changes

- Update the README's future-tense and outdated completion statements.
- Add four comparison grids with columns for upstream capability, current
  guile-dmenu status, policy, and notes.
- State in the introduction that the raw
  [dmenu contract](https://stagit.suckless.org/dmenu/file/dmenu.1.html)—stdin/stdout,
  exit status, basic selection semantics, and lean execution—is foundational.
- Treat Wayland protocols as implementation constraints rather than
  compatibility targets, particularly for placement, monitors, pointer/touch,
  clipboard, scaling, and
  [composed text input](https://wayland.app/protocols/xx-text-input-v3).
- Validate comparison claims against the current
  [bemenu interface](https://raw.githubusercontent.com/Cloudef/bemenu/master/man/bemenu.1.scd.in),
  [Emacs `completing-read`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Completion.html),
  [Emacs completion styles](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html),
  and
  [Guile readline behavior](https://www.gnu.org/software/guile/manual/html_node/Loading-Readline-Support.html).

## Support Policy and Backlog

### Bemenu

- Supported now: vertical filtering and selection, prompt/prefix, row limit,
  wrapping, fixed height, line height, border, and the implemented color subset.
- Backlog under "rich vertical":
  - vertical up/down layout;
  - positioning, monitor selection, margins, width, and panel avoidance through
    suitable Wayland protocols;
  - scrollbar, counter, password display, fonts, cursor sizing, complete colors,
    border radius, and text overflow;
  - initial filter/index, automatic and single-item selection, empty-input
    handling, and single-instance behavior;
  - pointer/touch, clipboard operations, richer editing bindings, and
    multi-selection.
- Permanently unsupported:
  - horizontal layout;
  - X11 and curses backends;
  - `bemenu-run`, command execution, forking, and custom execution wrappers;
  - Vim modal bindings;
  - strict bemenu CLI, environment/configuration, library, or ABI compatibility.

### Emacs `completing-read`

- Record the positional call contract, collection types, predicates, match
  confirmation, programmed tables, boundaries/metadata queries, cursor editing,
  defaults, histories, and substring style as implemented.
- Backlog the rich completion engine:
  - ordered style pipelines and standard useful styles;
  - category-aware behavior;
  - metadata-driven sorting, annotation, grouping, and affixation;
  - completion help, cycling, and multiple-value reading;
  - richer history persistence and navigation.
- Permanently exclude exact Emacs minibuffer/window behavior, buffer and
  text-property machinery, Emacs keymap/global-variable semantics, and binary
  integration with Emacs or Guile-Emacs.
- State prominently that rich completion edge cases must not materially slow
  ordinary `dmenu` use. Keep the raw string-list path free of unnecessary style
  traversal, metadata queries, question-state construction, and rich history
  work. Do not add a numeric benchmark gate yet.

### Codex-like questions

- Distinguish this interaction model from the existing Codex
  `PermissionRequest` integration.
- Record the implemented foundation: validated question and option records,
  stable ids, one-to-three-question batches, two or three choices per question,
  choice descriptions, Tab-triggered free-form comments attached to a selected
  choice, recommended markers, retained answers, Back navigation,
  graphical execution, collision-proof index-to-id selection with input
  filtering disabled, navigation controls outside the option-id namespace,
  recommended or retained initial selection, opt-in free-form Other answers
  tagged distinctly from option ids, one monotonic deadline shared by every
  page and Other prompt, id-to-answer results, and structured outcomes for
  answered, user-cancelled, timed-out, window-closed, and graphical-failure
  termination. The compatibility wrappers still return an answer or `#f`.
  Timeout may optionally resolve unanswered questions from their unique
  recommended choices when the caller explicitly enables it; supplied answers
  are retained, and a missing recommendation leaves the outcome timed out.
  The ~confirm/result~ convenience API builds on those same structured result
  and failure semantics, while ~confirm~ provides a compact boolean wrapper.
  The ~guile-dmenu-questions~ command provides the generic non-Scheme boundary:
  one structured JSON request on stdin, only the answer/outcome on stdout, and
  diagnostics on stderr, with no prompt contents in argv or environment.
- `codex-dmenu-approval` now uses the structured prompt API, so the real
  Codex-facing consumer exercises stable option ids and structured outcomes
  without changing its fail-safe empty-output fallback contract.
- The packaged structured command now passes a real headless River session:
  timeout and a multi-page interaction exercise retained-answer replacement,
  Back navigation, free-form Other input, stable-id JSON serialization, clean
  stderr, and continued Wayland protocol health. This is sufficient for a pure
  Codex-style various-choice hook replacement wherever Codex exposes a hook.
- Do not claim transparent replacement of ordinary Codex questions until Codex
  exposes an integration point for redirecting them. The currently documented
  `PermissionRequest` hook covers approval requests only; this external product
  limitation is separate from graphical widget maturity.
- Apply fail-safe prompt requirements across this work: distinguish denial from
  cancellation, avoid displaying or logging secrets unnecessarily, and keep
  sensitive content out of argv and environment variables.

### Guile readline

- Present comparison only; readline is not a compatibility target and creates
  no new backlog by itself.
- Show existing overlap: insertion, cursor movement, backspace/word deletion,
  completion, history traversal, and key repeat.
- Show missing readline functionality: standard control/meta bindings,
  delete-at-point, word movement, kill/yank, undo, history search and
  persistence, configurable keymaps, Vi mode, macros, and numeric arguments.
- Explain non-conflict: guile-dmenu uses explicit per-session Wayland state,
  while `(ice-9 readline)` is a process-global, terminal-oriented,
  non-reentrant interface.
- Do not plan direct code reuse, adapters, `.inputrc` support, terminal-port
  integration, or Guile readline API parity.

## Roadmap Order and Validation

1. Preserve the lean raw-dmenu contract whenever shared completion state is
   refactored.
2. Finish the generic paged confirmation/question API and its correctness,
   outcome, command-boundary, and Codex-approval integration gates.
3. Expand the rich `completing-read` engine.
4. Add selected rich vertical bemenu-derived capabilities.
5. Keep readline as a documented comparison, not a milestone.

Acceptance checks:

- Every feature in the four grids has exactly one status: supported, backlog,
  or intentionally unsupported.
- README and roadmap no longer describe implemented completion work as future
  work.
- Unit-suite status is documented from the current 275 passing Scheme tests
  plus the approval command regression test; no
  integration or package-build claim is made without running those checks.
- Structured-question unit tests cover Back/Next state retention, recommended
  default selection, free-form answers, automatic resolution, global deadline
  exhaustion, and every structured termination outcome.
- Collision tests cover duplicate display labels, labels containing the
  recommendation suffix, and labels identical to Back/Next controls; every
  answer must still resolve through its stable option id.
- Command-boundary tests cover valid JSON, malformed input, stdout purity,
  diagnostics, cancellation, and failure outcomes without leaking prompt data
  through process arguments or environment variables.
- The headless Wayland integration test exercises a real multi-page question,
  Back with retained-answer replacement, free-form Other input, timeout, clean
  stderr, and continued protocol health instead of relying only on an injected
  reader. Add a real compositor-driven window-close case. Compositor failure is
  already covered at the executable boundary with a missing display, and both
  close and failure outcomes remain covered by structured unit tests.
- The Codex approval integration is regression-tested for allow, deny, review
  in terminal, deadline exhaustion under lock contention, malformed hook
  input, and graphical failure. All fallback cases leave stdout empty, and
  malformed sensitive input is absent from diagnostics. A real graphical
  timeout remains part of the headless Wayland gate.
- A Guix package build verifies that the question modules, generic command, and
  migrated approval command are installed and runnable from the packaged
  environment.
- Future rich-completion tests cover style ordering and metadata while ensuring
  the ordinary menu path does not invoke those rich mechanisms.
- No benchmark is introduced in this pass; performance remains an explicit
  architectural and review constraint.
