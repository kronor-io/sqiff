# Readable code in sqiff

How sqiff's code is meant to read, and how that is checked. These rules came out of
refactoring code that reviewers found unreadable: the examples are real, from
`git log`.

They are review rules, not laws. Each says when not to apply it. A change applies
them to the code it touches; it does not clean up unrelated code on the way.

## General rules

### R1. Show an operation's stages as named steps

A function that groups, decides, names and renders in one body makes the reader
hold all of it at once. Split it so its body reads like a table of contents over
named functions with signatures.

```haskell
-- before: one call and a 140-line where block
units ... steps = nameMigrations (0 :: Int) (claimReverts (foldr (merge . single) [] steps))
  where ...

-- after
units names conventional inverseCtx forward sqitchPlan now =
    nameMigrations names now
        . claimReverts inverseCtx forward
        . groupSteps names conventional sqitchPlan
```

A large `where` block is the symptom; the problem is the hidden dependencies and
the several jobs in one place. Do not split a short, linear operation, or extract
pieces you can only name `go2` or `process`. Small local helpers are fine.

### R2. Name bundles whose positions carry meaning

A tuple whose elements are told apart only by position, or a list of positional
arguments threaded through a recursion, becomes a record.

```haskell
-- before
readBoth :: ... -> RIO AppEnv ((Schema, Dependencies), (Schema, Dependencies), Map ObjectKey BodyCheck)
-- after
readBoth :: ... -> RIO AppEnv ComparisonInputs
```

Others: `Surrounding{dropBefore, createAfter}` for a `([Text], [Text])`, and
`ScanState` for the scanner's seven arguments. Leave conventional pairs, map
entries and short local intermediates alone. A record that grows into a `Context`
holding everything hides dependencies as well as a closure does.

### R3. Record a fact where it becomes known

When code works out what something means from incidental data — English text, a
rank number, a substring of generated SQL, a message prefix — store the fact
explicitly where it is first known.

```haskell
-- before: two places guessed what a step was for
| " rename column " `T.isInfixOf` s.sql = "rename"
mainStep b = take 1 [n | ..., s.rank == createRank b]
-- after: the builder that makes the step says
| s.role == RenamesColumn = "rename"
mainStep b = take 1 [n | ..., defines s]
```

Others: the `ConversionMayFail` hazard (matched by its text before), and
`Outcome = Fails | NotChecked` for a body check (read from `"not checked: "`). A
`Maybe` that means something other than absence, such as `Nothing` for "delete
this", becomes a sum type (`StatementEdit = ReplaceStatement Text | RemoveStatement`).

This does not forbid reading text: parsing input from outside — a deploy file,
the catalog — is what the parsers are for. The rule is about reading meaning back
out of our own output.

### R4. Decide first, present after

When a decision means something on its own, make it a value, and render that value
in a separate function.

```haskell
usageOf :: Effects -> Evidence -> ObjectId -> Usage   -- what happens to each dependent
renderUsage :: (ObjectId -> Text) -> Usage -> [Text]  -- the only place that knows the headings
```

Others: `planRewrite` then `applyRewrite`, and `grantEdits` then `grantEditSql`.
A one-line message with no reusable decision behind it does not need a type.

### R5. Keep pure rules callable outside the IO

Policy buried in transaction or connection code can only be tested by running a
database. Pull it out as a pure function that the IO calls.

```haskell
editTable ... = do
    (current, desired, afterResult) <- inRolledBackTransaction ...
    pure (compareTableSchemas baseline current desired, afterResult)
```

Others: `classifyBodyResults`, and `keepLiveSettings` with `ignoredChangeReason`.
Do not split code where the order of effects is the point.

### R6. Name the questions the code keeps asking

`Map.findWithDefault Set.empty object deps.exact` written in four places is one
question: `exactDependentsOf object deps`. Name it before reaching for a new data
structure.

### R7. Write down the rules a change relies on

A refactor that depends on an invariant — every retyped column is a key of
`recreatedAround`, a `Set` keeps the first error per routine — states it beside
the code that produces the value. Comments explain contracts, order and surprises;
they do not restate the code, and not every helper needs one.

## Haskell

### H1. Build records with many fields by field name

```haskell
-- before
Step CreatePhase Creates 6 object sql Shared [] key change UndoObject
-- after
(createStep object sql){rank = 6, change = Changed old new}
```

A constructor with two or three obvious fields can stay positional. A test helper
with defaults must not hide the field the test is about.

### H2. Give each real stage a top-level signature and explicit inputs

A recipe that captured `ctx`, `key` and `change` from a 460-line function became
`indexRecipe :: Target -> QName -> [Step]`. Keep small helpers local when their
place explains them, and mutually recursive workers together.

### H3. Plain data and functions before abstraction machinery

Reach for a record, a sum type or a named function first. A lens library, a class
hierarchy or a generic framework has to justify itself by making its callers
simpler. sqiff has not needed one. Ordinary `traverse`, `foldMap` and `Semigroup`
are welcome.

## sqiff

- A `Step` carries its `Role`. Code that orders, groups, reverts or verifies steps
  reads the role, never the rank or the SQL. The rank only orders steps.
- Output is a contract: plans, sqitch files and the table report. A refactor keeps
  them byte for byte unless the change is meant to alter them.
- Record fields are strict (`StrictData`). A field that has to stay lazy says so
  with `~` and a comment saying why.

## Changing code safely

A refactor that is meant to keep behaviour:

1. **Pins the behaviour first** where tests do not: exact-output tests for the
   scanner and the rewrite came before their refactors.
2. **Says what it preserves**: output order, which duplicate wins, error
   classification, evaluation and when resources are released.
3. **Compares the old and new binaries** on the fixtures (`sqiff sqitch`) and on
   `sqiff table` scenarios, when tests do not cover the path.
4. **Benchmarks against a fixed commit**, with `-O1` builds, interleaved, in paired
   rounds, recording the commit SHA, the build options and the data. Never against
   a branch name: a stale local `main` once made the branches look twice as costly.
5. **Goes in one commit per idea**, so each can be reviewed and reverted alone.

## What is not enforced

Function length limits, bans on `where`, tuples, `Bool` or list comprehensions, a
comment on every helper, a type for every renderer, every HLint suggestion, and
performance thresholds in CI. Each of these would be met by worse code.

## How the rules are checked

| Rule | Check |
|---|---|
| H1, H2 (signatures), exhaustive matches | GHC: `-Werror` for missing signatures, missing fields, incomplete patterns and partial record selectors and updates (`sqiff.cabal`) |
| R3 for steps | HLint: no reading a step's `.sql` (`.hlint.yaml`), with the known exceptions listed there |
| R6 | HLint: `exactDependentsOf`, `routinesNaming` |
| Layout | fourmolu (`fourmolu.yaml`) |
| Everything else | Review, with `.claude/skills/readability-review` |

`scripts/check fast` runs fourmolu and the HLint gate; `scripts/check full` also
builds and runs every test. `scripts/check advice` prints HLint's other
suggestions without failing.

A rule moves from review to a check only when review keeps missing it. Checks
that need the syntax tree — positional construction of records with many fields,
tuple-heavy signatures, where-block size — would go in a checker built on
`ghc-lib-parser`; [hopinion](https://github.com/NorfairKing/hopinion) is a
candidate framework for one. Existing code would be grandfathered with a recorded
baseline, so only new or growing findings fail.
