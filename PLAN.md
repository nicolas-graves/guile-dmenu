# Emacs-Shaped `completing-read` for Guile

## Summary

Extend the graphical library from a list picker into an Emacs-shaped completion API while preserving existing dmenu and Codex behavior through an explicit menu-selection mode. Support the full positional call contract, Guile-native completion tables and histories, programmed completion, editable initial input, defaults, matching requirements, and input transformation. Build a pluggable style framework, with case-insensitive substring matching as the only built-in style for now.

## Public API and Behavior

- Change the procedure shape to:
  ```scheme
  (completing-read prompt collection
                   [predicate [require-match [initial-input [history
                   [default [inherit-input-method]]]]]]
                   #:key message input-enabled? timeout max-message-lines
                         selection-mode completion-style)
  ```
- Default `selection-mode` to `'text`:
  - RET submits the literal input subject to `require-match`.
  - Empty input returns the first default, or `""` without a default.
  - TAB attempts completion; a unique match is inserted, while multiple substring matches only extend a shared prefix when possible.
- Add `'menu` selection mode for existing callers:
  - RET returns the highlighted row exactly as today.
  - Update `dmenu` and `codex-dmenu-approval` to request this mode explicitly.
- Preserve `#f` for cancellation, window closure, timeout, or Wayland failure.
- Accept collections as string lists, alists, vectors, hash tables, or programmed-completion procedures. Alists return their string keys.
- Programmed tables receive `(input predicate action)` with Guile equivalents of Emacs actions: `#f` for try-completion, `#t` for all completions, `'lambda` for exact-match testing, and metadata/boundary action forms.
- Apply predicates to the underlying collection entry, not merely its display string.
- Implement `require-match` values `#f`, `#t`, `'confirm`, `'confirm-after-completion`, procedures, and other truthy values according to Emacs behavior.
- Accept initial input as a string or `(string . zero-based-position)` and maintain an actual cursor position.
- Export a mutable `<completion-history>` API. Accept a history object or `(history . one-based-position)`; `#t` disables recording. Record successful text submissions, suppress consecutive duplicates, and enforce an exported configurable history length.
- Accept a single default or list of defaults; expose defaults through empty submission and forward history navigation.
- Export:
  - `completion-ignore-case?`, defaulting to `#t`.
  - `completion-input-transformer`, defaulting to identity and applied only when `inherit-input-method` is true.
  - A completion-style registration/lookup API. Ship only the `'substring` style in this milestone.

## Implementation Changes

- Extract a pure completion engine from the Wayland loop. It will normalize collections, invoke programmed tables, filter candidates, test exact matches, perform completion, enforce submission rules, and update history.
- Expand completion state with cursor position, confirmation state, history position, whether completion was invoked, and the active normalized candidates.
- Extend keyboard handling for TAB, Left/Right, Home/End, cursor-aware insertion and backspace, and history/default navigation. Keep current Up/Down candidate navigation and cancellation bindings.
- Render the cursor at its actual text position. Confirmation-required submissions should retain the input and show an inline status/message instead of closing.
- Keep the Wayland event loop responsible only for translating events into pure state transitions, redrawing, and returning the engine’s final result.
- Document the positional API, supported Guile collection representations, programmed-table action protocol, history object, substring-only style status, cancellation behavior, and the difference between text and menu modes.

## Test Plan

- Add SRFI-64 unit tests for collection normalization across lists, alists, vectors, hash tables, predicates, and programmed tables.
- Test substring matching with case-insensitive default and case-sensitive parameterization, unique completion, ambiguous completion, exact-match detection, and no matches.
- Test every `require-match` form, including both confirmation modes and procedure acceptance.
- Test empty input, scalar/list defaults, initial cursor positions, insertion/deletion in the middle of input, and transformed input.
- Test history initialization, forward/backward navigation, default navigation, recording, duplicate suppression, and length limits.
- Test text-mode RET versus menu-mode RET so existing dmenu/Codex selection behavior cannot regress.
- Retain the headless River smoke test and add pure event-transition coverage for keyboard behavior that the current River harness cannot inject.
- Verify with the new unit-test runner, the existing `script/integration-test`, and a Guix package build.

## Assumptions

- “Emacs-compatible” means an Emacs-shaped Guile API and behavior, not binary integration with a particular Guile-Emacs runtime.
- Substring is the only implemented completion style and remains case-insensitive by default; the style interface permits later prefix/basic, partial, flex, or orderless implementations.
- Guile history records replace Emacs’s implicit mutation of symbol-bound global variables.
- Wayland already supplies Unicode text; input-method inheritance is modeled through the ambient transformer parameter.
- Pointer support, richer text rendering, and unrelated dmenu compatibility work remain out of scope.
