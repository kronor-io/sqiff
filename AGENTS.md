# Working on sqiff

Read `docs/readability.md` before changing Haskell code. Apply its rules to the
code you change; do not clean up unrelated code.

- Prefer explicit domain data, named stages and pure rules callable outside IO.
  Small local helpers, ordinary tuples and standard abstractions are fine.
- Never decide what a `Step` does from its rank or its SQL: read its `role`.
- When a change is meant to keep behaviour, say what it preserves (output order,
  which duplicate wins, error classification, evaluation, resource lifetimes) and
  check it, as `docs/readability.md` describes.
- Run `scripts/check fast` while working and `scripts/check full` before
  committing, inside `nix develop`. Report the commands you ran and anything you
  could not run.
- Do not update expected outputs, add HLint exceptions or relax warnings just to
  make a check pass. An exception needs a reason written beside it.
- Do not add an abstraction only to satisfy a rule or a metric.

For a review of a change, follow `.claude/skills/readability-review/SKILL.md`.
