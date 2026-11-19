---
name: readability-review
description: Review a sqiff change against docs/readability.md and report concrete findings. Use when asked to review a diff, branch or PR for readability, or before opening a PR that refactors code.
---

# Readability review

Review a change against `docs/readability.md`. The checks in `scripts/check`
already cover what a tool can decide; this review covers the rest.

1. Read `docs/readability.md`.
2. Find the change: the diff against the base the user names, or else against
   the branch's merge base with `origin/main` (`git merge-base HEAD origin/main`).
   Say which base you used, by SHA.
3. Read each changed function in full, and the callers of any whose type or
   behaviour changed.
4. For each problem, report:
   - the rule (R1–R7, H1–H3, or the sqiff section) and `file:line`;
   - what is hard to read, or what invariant is at risk, concretely;
   - the smallest change that fixes it.
5. Sort the findings into **defects** (behaviour or a stated invariant at risk),
   **worth doing** (a rule clearly applies) and **optional** (taste). Check each
   against the rule's "when not to apply" first, and drop it if that applies.
6. If the change is meant to keep behaviour, say whether it states what it
   preserves and how that was checked, and what is left unchecked.

"No findings" is a valid result. Do not pad the review, score the code, or
suggest cleanups outside the change. Do not edit files unless asked to.
