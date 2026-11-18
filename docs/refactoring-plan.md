# sqiff: review and refactoring plan

Sources: a full static review by Codex (`docs/codex-review.md`, 29 findings) plus my own
read of the code. I spot-checked Codex's main correctness claims against the source, and they hold.

## Diagnosis in one sentence

**Rendered SQL text does every job at once.** It is the catalog serialization, the
comparison key, the migration operation and the presentation format. Every module then
re-parses it with `T.isPrefixOf` / `T.replace` / `T.words`, and most bugs, as well as most of
the unreadability, come from that.

Concretely:

| Smell | Where | Consequence |
|---|---|---|
| `Address { objectType :: Text, objectNames :: [Text], objectArgs :: [Text] }` is the identity everywhere | Catalog, Diff, app | `"table column"`, `[s, t, c]` pattern matches scattered across ~40 sites; `Object` ADT exists but is decoded ad hoc and re-encoded |
| `Schema = Map ObjectKey Text`, `Change = Added Text \| …` | Diff, Recipe, Plan | definitions are re-parsed (`parseColumn`, `referencedTable`, `aclParts`, `"partition by "`, `"BEGIN ATOMIC"`, `" ON "`) |
| Bare `Text` for connection strings, change names, revisions, SQL fragments, identifiers | everywhere | `""` means "libpq env"; passwords printed in errors; quoting is by convention |
| `CatalogId = (Int64, Int64, Int32)`, `pgProc = 1255` | Catalog, app | magic numbers, positional tuples |
| Positional records: `Connected conn extra schema addrs tempTables`, `type Retyped = (QName, Text, Address, Text)`, `readBoth` returning a tuple of tuples | app | unreadable destructuring |
| Effects inside data: `IO (Maybe Text)` fields in `Connected`, `Dependents`, `PlFunctions` | app, Catalog | unclear caching/lifetime |
| `app/Sqiff.hs` is 1239 lines: CLI, dependency discovery, concurrency, planning policy, branch replay, writing | app | untestable, nothing reusable |
| `Recipe.definition` is one ~400-line function | Recipe | hard to read and extend |
| No test suite at all | n/a | every refactor is blind |

## Verified correctness bugs (highest value)

1. **Plan order:** sort key is `(phase, rank, topoPosition)`, so kind rank beats real dependencies (`Plan.hs:29`): a function that reads a new view is created before the view.
2. **Cycles:** `topoOrder` silently appends cyclic nodes (`Plan.hs:124`), so the plan looks valid but fails.
3. **`affected`** drops a dependent if *any aspect* of it changed (`Diff.hs:55`), e.g. a comment edit hides a view that blocks a retype.
4. **ACLs:** `"role privilege"` is split on the first space, so role names with spaces break (`Recipe.hs:622`); null ACL and empty ACL collapse to `default`; new objects keep PUBLIC defaults; grant-option removal revokes the whole privilege.
5. **Text rewriting inside literals:** `tidyIndex` / `tidyConstraint` keyword replacement, `replayable` stripping `concurrently ` anywhere, and `fileDrift` stripping ` not valid` anywhere (comments included). The aside-table rename does a global `sqiff_aside.` replace.
6. **Column description** is newline-delimited, so multi-line defaults or generated expressions are truncated or misparsed (`column.sql` / `parseColumn`).
7. **Fixed `$$` delimiters** around interpolated names, plus a `RAISE` format string built from an index name.
8. **Sqitch layout overrides** (`--plan-file`, `--top-dir`) are not passed to the `sqitch` subprocesses that write.
9. **Name collisions:** overloads share one conventional change name, and the per-day tag can collide.
10. **`plan` cannot retype a column under a view:** the drop-and-recreate handling (`viewsAroundTypeChanges`) exists only in the `table` command, so `plan` emits a bare `alter column … type` that PostgreSQL refuses. Found by the integration tests.
11. **Resource lifetimes:** connections are never released, and the `async` calls are not scoped.

The full list, with P1/P2 severity, is in `docs/codex-review.md`. One Codex claim did not hold up:
`CREATE OR REPLACE VIEW` without a `WITH` clause does reset the old view options, so
removing an option works (`test/fixtures/view-option-reset` keeps it that way).

Phase 0 encodes these bugs as tests expected to fail: `known bug:` unit tests and fixtures
with a `known-bug` file. When a fix lands, its test turns into an "unexpected success", which
is the signal to delete the marker.

## Target architecture

```
Sqiff.Sql.*        Identifier, QName, Literal, SqlExpr, Statement: quoting lives here only
                   Lexer (one shared, span-aware; Script/Rewrite/TableEdit use it)
Sqiff.Model.*      ObjectId (sum type, parsed once from pg address), Aspect, Definition (typed per kind),
                   ColumnDef / Acl / IndexDef / ConstraintDef …, Schema
Sqiff.Catalog.*    DB boundary: queries → Model (decoders return Either, unknown → Unsupported)
Sqiff.Graph        one dependency graph: closure, column-aware reachability, topo sort returning cycles
Sqiff.Diff         Change a = Added a | Dropped a | Changed a a  (Functor/Foldable)
Sqiff.Plan.*       Operation ADT, per-kind recipe modules, scheduling over an operation DAG
Sqiff.Render.*     Operation → SQL text; Sqitch artifact preparation (pure)
Sqiff.Sqitch.*     Plan parser (structured), Layout, installer (effectful, staged)
Sqiff.App.*        AppEnv (RIO), workflows: Describe, Diff, Plan, SqitchWrite, TableEdit, Baseline
app/Main.hs        CLI parsing into a validated Command, then runRIO
```

Guiding rules:

- **Parse, don't validate:** catalog rows, YAML, CLI flags, plan files and name templates are
  each decoded once into types that cannot represent invalid states (`NameTemplate` AST,
  `ConnectionString` with redacted `Display`, `ChangeName`, `Revision` vs `CommitId`,
  `NonEmpty Connection`, `ScratchSession` / `ReadOnlySession` wrappers).
- **Hidden constructors** for `Identifier` and `SqlExpr`. Quoting happens in exactly one place.
- **Pure core, effectful shell:** planning and rendering stay pure. IO happens in `Sqiff.App`
  via an `AppEnv` with `HasLogFunc`, a timing capability and bracketed resources.
- **Hazards, txn mode, verify and inverse are derived from `Operation`,** not from SQL substrings.

## Progress

- [x] **Phase 0:** test suite (`383bf3a`).
- [x] **Phase 1:** typed identity (`Sqiff.Identifier`, `Sqiff.Model`, `CatalogId`,
  `ConnectionString`). Output is unchanged and every golden file matches. Notes:
  - Interpolating an `Identifier`, `QName` or `TypeName` with `[i|...|]` is a compile
    error. Use `quoted` / `qualified` for SQL and `nameText` / `dotted` for messages.
  - The catalog never describes foreign tables, operators, casts, rules or collations,
    so those paths stay `Unknown` and are effectively dead. Phase 3 can delete them.
- [x] **Phase 2 (part 1):** typed definitions for columns and privileges. `Schema` maps
  keys to a `Definition` (`ColumnDefinition`, `AclDefinition`, or `Printed` text for the
  other kinds), and `Change a` is polymorphic. The column and privilege queries return
  JSON, decoded once in `Sqiff.Catalog`. A description that does not decode is an error.
  `printed` renders the old text, so `diff` output is unchanged.
  - Fixed: role names with spaces, multi-line defaults, PUBLIC `EXECUTE` on new
    functions, grant-option removal (`revoke grant option for`), null vs. empty ACLs, and
    `fileDrift` ignoring "not valid" inside comments.
  - Found: grants have no grantor, so a grant option another role has used cannot be
    revoked (`test/fixtures/grant-option-removed`).
- [x] **Phase 2 (part 2):** typed tables (`TableDef`), views (`ViewDef`), indexes
  (`IndexDef`) and constraints (`ConstraintDef` with kind, validity, columns and foreign-key
  target). No recipe searches SQL text for `" ON "`, `"FOREIGN KEY "`, `"REFERENCES "`,
  `"partition by "` or `"with ("` any more, and `referencedTable` and `indexTableName` are
  gone. Output is unchanged: all golden files match, and five new fixtures (partitions,
  table settings, unique constraints, validation, materialized views) give the same
  output before and after.
  - Still printed text: routines, types, sequences, extensions, triggers, policies and
    comments. Sequences (`" owned by "`), extensions (`"version … schema …"`) and enum
    labels are the remaining recipes that parse text. They are small and good candidates
    to type next. Routine bodies stay text by nature.

## Phases (each one builds, passes tests, and ships on its own)

**Phase 0: safety net.**
Add a `test-suite`: pure unit tests (quoteIdent, `match`/`fill`, topoOrder, Script splitting,
Rewrite) and an integration harness that starts a throwaway PostgreSQL 18, applies
`from.sql` / `to.sql` fixture pairs, runs `plan`, applies the plan to `from`, and asserts that
`diff` is then empty. That round-trip property catches most planner bugs. Capture golden
outputs of the current behaviour first.

**Phase 1: typed identity (the parse-don't-validate spine).**
`Identifier`, `QName`, a typed `ObjectId` sum replacing `Address` everywhere above the catalog
boundary, a `CatalogId` record, `ClassOid` constants, and `ConnectionString`. This is
mechanical but touches every module, and it removes most stringly pattern matching.

**Phase 2: typed definitions.**
`ColumnDef` (via structured or JSON catalog output), `Acl` / `Grant`, `IndexDef`,
`ConstraintDef`, `ViewDef` options. `Change` becomes polymorphic. This fixes bugs 4 and 6 and
removes the definition re-parsing.

**Phase 3: dependency-correct planning.**
An `Operation` ADT, one `Graph` module, scheduling over an operation DAG that uses rank only
as a tie-break, cycles reported as `Left`, and coordinated serial↔identity changes. Fixes bugs
1 to 3.

**Phase 4: rendering and literals.**
A single literal/dollar-quote policy and token-aware formatting, with no global
`T.replace`. Fixes bugs 5 and 7.

**Phase 5: app layer.**
Split `app/Sqiff.hs` into `Sqiff.App.*` workflows, add `AppEnv`, bracket connections, use
`withAsync`, and validate the CLI into a `Command` up front. Split `Catalog` and `Sqitch`.

**Phase 6: sqitch output.**
A complete `Layout` passed to every subprocess, batch validation (names, tags, paths), and
staged writes. Fixes bugs 8 and 9.

Later: baseline locking and cache fingerprinting, consistent catalog snapshots, and Folder
export collisions.

## Notes

- The project needs GHC 9.12.4 (`cabal.project.freeze`); use the flake's dev shell.
- PostgreSQL 18.6 and sqitch-pg 1.5.2 are available in the store, which is enough for the
  integration harness.
