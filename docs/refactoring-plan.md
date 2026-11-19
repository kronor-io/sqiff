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
- [x] **Phase 2 (part 3):** typed sequences (`SequenceDef`), extensions (`ExtensionDef`)
  and types (`TypeDef`: enum, domain, composite, range). No recipe parses catalog text any
  more. Fixed: enum labels written as `E'...'` with ", " inside
  (`test/fixtures/enum-tricky-label`).
  - Still printed text: routines, triggers, policies, schemas and comments. Their
    recipes mostly emit the text as it is. They still read routine headers and bodies
    (`BEGIN ATOMIC`, `RETURN`) and rewrite `CREATE TRIGGER` to `CREATE OR REPLACE TRIGGER`.

- [x] **Phase 3:** dependency-correct planning.
  - `Sqiff.Graph` holds the shared graph code: `reachedFrom`, `topoOrder`, and `schedule`,
    a topological sort with priorities that reports what a cycle blocks.
  - Steps are scheduled over a dependency graph. Edges are hard constraints, and kind
    rank, object order and recipe order only break ties among ready steps. A dependent
    waits for an object's main step, not for its settings, grants or comment. A new
    table's columns pass their dependencies to the table's step. A sequence's ownership
    waits for its column; the sequence itself does not.
  - Routines rank after views. PostgreSQL records what a view uses, but not what a
    function's string body uses.
  - `plan` and `planWith` return `Either DependencyCycle [Step]`, and the commands fail
    with the objects involved. Before, a cycle was silently appended.
  - `affected` excludes only dependents whose definition is in the diff.
  - The view handling around retyped columns moved to `Sqiff.Recreate`, and `plan` and
    `sqitch` use it. `--from` reads the transitive dependents of what changes.
  - Fixed: function-needs-new-view, retype-column-under-view, and two new fixtures that
    fail on the previous commit: table-default-uses-new-function and
    retype-column-under-nested-views.
  - Not done: serial-to-identity as one coordinated operation, and reverts per unit
    (Codex 11 and 16).

- [x] **Phase 4:** one SQL-text policy, in `Sqiff.Sql`.
  - The lexer from `Rewrite` (`regions`: code, quoted or comment, per character) is shared
    by `Rewrite`, `Sqitch`, `Script` and the app.
  - `replaceInCode` rewrites words only where they are code. It is used for index and
    constraint tidying, `IF NOT EXISTS`, replaying `concurrently` and line-breaking
    foreign keys.
  - `literal` follows `quote_literal` (an `E''` string when there is a backslash) and
    replaces four copies.
  - `dollarQuoted` and `doBlock` choose a tag the body does not contain. They replace
    the fixed `$$` and `$sqiff$`.
  - Invalid-index checks use `raise exception using message = …`, so `%` in a name is
    safe.
  - `fill` expands a template in one pass, and `ROLLBACK WORK/TRANSACTION TO` is kept.
  - No `known bug:` unit test is left. The new fixture `index-odd-name` fails on the
    previous commit.
  - Kept global: `TableEdit` still replaces `sqiff_aside.` everywhere in a definition,
    because owned sequences move with the table and their names appear in
    `nextval('…')` literals. `Script.splitStatements` keeps its own state machine, which
    tracks `BEGIN ATOMIC` depth.

- [x] **Phase 5:** app layer.
  - `app/Sqiff.hs` is now about 40 lines: it parses the command and dispatches. The
    workflows are library modules:
    - `Sqiff.App.Compare`: describe, diff, plan and sqitch.
    - `Sqiff.App.Read`: describing databases and finding edges.
    - `Sqiff.App.Dependents`.
    - `Sqiff.App.Table` with `.Env`, `.Findings` (pure) and `.Rewrite`.
  - `Sqiff.App.Env`: `AppEnv` (configuration, and the scope with its excluded schemas)
    for `RIO`, plus `say` and `note`.
  - The CLI uses opt-env-conf (`Sqiff.App.Options`). The table flags parse into a
    `TableMode`:
    - `MigrateToFile` takes a `Comparison` (`WithBaseFile renames | WithLiveTable drops`).
    - `RewriteFile` takes an optional `--live-from` database.
  - Flags that do not apply together fail in the parser (`checkMapEither`), with exit
    code 1, before anything connects. `--allow-drops` without `--against-database` and
    the file-comparison flags with `--rewrite-file` used to be ignored silently.
  - Every connection is scoped (`withConnection`, `withConnections`), and every
    background read uses `withAsync`.
    - `withDatabase` opens the connections and describes the database the first time
      the workflow asks (`onDemand`).
    - All of it is released when the workflow ends or fails.
    - A smoke run against a throwaway cluster gave the same output as the previous
      commit, with no connections left open.
  - `Catalog` is split:
    - `Sqiff.Connection`: connections, `UserError`, `run`.
    - `Sqiff.Catalog.Rows`: catalog ids and codecs.
    - `Sqiff.Catalog.PlpgsqlCheck`.
  - `Sqitch` is split:
    - `Sqiff.Sqitch.Plan`: the plan file and registry.
    - `Sqiff.Sqitch.Units`: pure grouping and rendering.
    - `Sqiff.Sqitch`: running sqitch.
  - `locateTable` normalises the deploy directory, so `--top-dir .` works.
  - The SQL-text unit tests from Phase 4 were never registered; they now run.

- [x] **Phase 6:** sqitch output. Fixes bugs 8 and 9.
  - `Sqiff.Sqitch.Layout` resolves the layout once:
    - It covers the top, deploy, revert, verify and reworked directories, the plan
      file and the script extension.
    - A setting for the target wins over the engine's, which wins over the core's, as
      in sqitch.
    - `--top-dir` and `--plan-file` override sqitch's configuration.
  - Every sqitch command sqiff runs gets `SQITCH_CONFIG` pointing at a generated file:
    - It has the project's own settings, with the layout pinned for the core, the
      engine and the target.
    - `add`, `rework` and `tag` have no top-dir option, so this is the only way to make
      them write where `--top-dir` says.
  - `Sqiff.Sqitch.Check` checks the whole batch before the first write:
    - Change names follow sqitch's rules, with no absolute path and no empty, `.` or
      `..` segment.
    - No name appears twice, no add is already in the plan, and no rework is missing
      from it.
    - Tags: a given tag must be new. The default is the first free one of
      `<day>_sqiff`, `<day>_sqiff_2`, …, so a second rework on the same day works.
    - `sqiff sqitch --dry-run` reports the same problems.
  - Adjacent conventional units with the same name merge, so overloads of a new
    function become one change instead of two `sqitch add`s of the same name.
  - Every write happens inside `withProject`:
    - sqiff's own writes go through a journal.
    - If anything fails, the files sqiff wrote are restored, the scripts and
      directories sqitch made are removed, and the plan is put back last.
    - The error then ends with "nothing was written, the project is as it was".
    - This covers `sqiff sqitch`, `table --write` and `table --rewrite-file --write`.
  - Tests:
    - Unit tests for the checks, the layout and the generated configuration.
    - `test/Writing.hs` runs the real sqitch (`SQIFF_TEST_SQITCH_BIN`): a custom
      layout, rollback after sqitch fails part-way, a second same-day tag, and a
      refused batch.
    - By hand: `sqiff sqitch` with `--top-dir`/`--plan-file` on nine fixtures, then
      `sqitch deploy`, then `sqiff diff`, which found nothing left.
  - Not done: reverts per unit (Codex 16) and serial-to-identity coordination (Codex 11).

- [x] **Column order of new tables (Codex 28, in part).**
  - `CREATE TABLE` used to list a new table's columns by name, so `created_at` came
    before `id`.
  - Each column now carries its `attnum` as a `Position`, and new tables list their
    columns by it.
  - A `Position` equals every other, so the same column in two databases that added
    their columns in different orders does not show as changed.
  - The rest of finding 28 (ownership, inheritance, range-type options) is open.

- [x] **Recreated objects keep their comments (Codex 10).**
  - A comment belongs to the object it is on, so dropping and creating an object again
    lost it. The diff saw no comment change, so nothing restored it.
  - The context now carries the desired database's unchanged comments (`keptComments`).
    Every recipe that drops and recreates an object restores those of what it dropped:
    - a materialized view, its columns and its indexes
    - an index rebuilt under a temporary name or dropped and created again
    - table constraints, with their index, and domain constraints
    - triggers and policies dropped and created again
  - The restoring steps carry the recreated object's key, so a sqitch unit's revert
    stays the inverse of the definition change.
  - The new fixture `recreate-keeps-comments` loses all eight comments on the previous
    commit.

- [x] **Verify scripts check what the migration did (Codex 27).**
  - Verify scripts run with an empty `search_path`, as sqiff reads the catalog. Columns
    are checked by `format_type(atttypid, atttypmod)`, so a change from `varchar(10)` to
    `varchar(20)` is verified, along with nullability and collation.
  - New checks for tables, routines (`to_regprocedure`), types (with an enum's labels),
    schemas, extensions and domain constraints.
  - Conventional changes get these checks too; they used to get an empty verify.
  - An object dropped to be created again by a later change is not checked as gone:
    `sqitch verify` runs every change's script on the final database.
  - A step sqiff cannot write (a `manual:` hazard) renders with a block that raises an
    error, so the deploy stops there until someone writes it. Before, it deployed as a
    change that did nothing.
  - Each fixture has a fourth test, the sqitch round trip:
    1. `sqiff sqitch` writes the plan into a new project.
    2. sqitch deploys it with `--verify` on a fresh copy of `from`, then runs
       `sqitch verify`.
    3. `diff` must then find nothing.
  - The round trip found the absence checks that fail after a later change recreates
    the object. By hand: the verify fails on a column still `varchar(10)` and passes once
    it is `varchar(20)`, and a manual step stops the deploy.

- [x] **Grants record their grantor (the last known bug, Codex 4/5).**
  - `Grant` has a `grantor`: `ByOwner`, or `ByRole r` for a privilege another role
    passed on. Owners need not match between databases.
  - PostgreSQL accepts `GRANTED BY` only for the current role, so a grant or revoke on
    behalf of another role runs as that role, in a `do` block with `set local role`.
  - Other roles' grants are revoked before the owner's and granted after them.
  - `grant-option-removed` passes, and its `known-bug` file is gone. The new fixture
    `grant-by-other-role` covers giving a grant as another role.
  - Chains of any depth are ordered by how far down the chain each grant is: revokes from
    the end of the chain, grants from its start (`grant-chain-removed`).

- [x] **`describe --out` file names (Codex 26).**
  - Names are escaped with `%` itself escaped too, so `a b` and `a-b`, or `/` and `%2F`,
    no longer share a file.
  - Paths that differ only in case get a hash of the object's key, since case-folding
    file systems would merge them.
  - An empty schema writes the folder and its marker.
  - `--replace` writes the new export beside the old one and swaps it in when it is
    complete, so a failure leaves the old export as it was.

- [x] **Failed probes and cleanups are no longer read as absence (Codex 24).**
  - `inRolledBackTransaction` runs every rolled-back check: the table comparison, the
    type-change and foreign-key probes, and the temp-table script.
    - When the check succeeds, a failed rollback is an error.
    - When it fails, its own error is reported.
  - The type-change probe and the temp-table script still return "unknown" when
    PostgreSQL refuses them, and sqiff falls back. Losing the connection now fails the
    command. Opening the temp-table connection is no longer wrapped in `tryAny`.
  - Body checks keep their per-body rollback. An exception there ends the command and
    releases the connection.

- [x] **Baseline locking and cache identity (Codex 22).**
  - `withBaseline` replaces `ensureBaseline` and keeps a lock in the baseline's cache
    folder for as long as the command uses the database:
    - It brings the database up to date under an exclusive lock, then uses it under a
      shared one, checking again after switching locks.
    - Runs that need the same revision share it; one that needs another waits, and says
      so.
  - The state file records the commit and a hash of the deploy settings from
    `sqiff.yaml` (compose file, service, user, database, environment, sqitch steps;
    not the password). A change in those settings rebuilds from an empty volume.
  - Run against Docker with a throwaway repository (`postgres:18`, sqitch on the host):
    - The first run built the baseline.
    - A second run reused it.
    - Two runs at once both succeeded, one waiting with the message.
    - Changing the baseline environment in `sqiff.yaml` rebuilt it from an empty volume.

- [x] **One snapshot per described database (Codex 25).**
  - `withSharedSnapshot` starts a repeatable-read transaction on the first connection
    and exports its snapshot; the others adopt it. `withDatabase` and `describe` read
    the catalog inside it, so the queries spread over four connections see one state.
  - The edges and plpgsql_check analysis that follow run outside it: the analysis
    creates temp tables and recovers from failed statements, which one long transaction
    would not allow. Edges whose ends were not described are dropped, as before.
  - A test shows two connections in a shared snapshot not seeing a table another
    connection created after it.

- [x] **Each migration reverts its own steps (Codex 16).**
  - `Step` has an `undo`:
    - `UndoObject` (the default) uses the object's inverse recipe. When the object's
      change is split over migrations, the one with its own drop recreates the old
      object, and the one with its own create drops the new one. A migration that only
      sets grants, indexes or comments on it reverts nothing, since redeploying it is
      repeatable.
    - `UndoBy` lists explicit steps. Concurrent index builds drop what they built. A
      temporary index name is renamed back. A dropped old index is built again with its
      comment. Validation and restored comments need nothing. A key attached with
      `using index` drops the constraint and builds its index again. The `table`
      command's column rename is undone by renaming back (formerly a special case in
      `writeMigration`).
  - A migration's revert uses a context in which only its own tables are created or
    dropped.
  - Reverting a dropped table also restores what went with it and had no steps of its
    own: constraints, indexes, triggers, policies, comments and grants.
  - Recreatable objects (materialized views, indexes, constraints, triggers, policies)
    restore their unchanged indexes, grants and comments when created, which only a
    revert's recreation ever has.
  - `sqiff sqitch` reverts recreate the views around a retyped column, as the plan does.
  - Functions a type makes for itself (a range type's constructors) are no longer
    described, so they are not planned on their own. The `types-added` goldens lose them.
  - Tests: the sqitch round trip now walks back from the last migration. Each is
    reverted, deployed again with `--verify`, and reverted again; reverting them all
    must give back `from`. Before these changes, 14 fixtures failed this walk.
  - The walk is skipped when a revert stops at a step sqiff cannot write (enum labels,
    resetting privileges), or when a `revert-gap` file names a change PostgreSQL cannot
    undo: `change-view` (`create or replace view` cannot drop a column) and
    `validate-constraint` (a validated constraint stays validated).

- [x] **Serial ↔ identity as one change (Codex 11).**
  - The column's recipe already converted in one `do` block that drops or creates the
    sequence and keeps its position. The sequence's own change ran too, and failed: the
    drop because the column default still used it, the create because the identity's
    sequence had the same name.
  - The context lists the sequences such a column change makes or drops
    (`convertedSequences`). Their recipe drops nothing, and for a serial only sets the
    sequence's options and owner after the column's change.
  - New fixtures `serial-to-identity` and `identity-to-serial` pass every round trip,
    revert walk included; both failed before.

- [x] **A view that loses, renames or retypes a column is created again.**
  - A view's description records its columns (name and type, `ViewColumns`). Like a
    column's position, they never make two views differ.
  - When the new columns extend the old ones, the plan keeps `create or replace view`.
    Otherwise it drops every dependent (views, materialized views, routines that hold
    them), drops and creates the view, restores its grants and comments, and creates the
    dependents again as the desired database has them, except ones the plan drops.
  - A recreation's restore steps (indexes, grants, comments) take the object's create
    rank and the recreation's key. They stay in its migration, so deploying or reverting
    that migration leaves the object whole.
  - In a split change, a migration's revert follows the drop and create phases only when
    the change has both. Otherwise the migration with the object's main step reverts it.
  - Views created again restore grants only when they have explicit ones, which drops
    the no-op "revoke what was granted" blocks from several goldens.
  - The new fixture `view-column-removed` passes every round trip. `change-view` loses
    its `revert-gap`, since its revert now recreates the view.

- [x] **How the `table` command recognises a table (Codex 21, in part).**
  - Finding the branch's migrations for a table also matches the table's name as
    migration names write it (lower-cased, other characters made dashes).
  - The check that `--live-from` has deployed every change touching the table now also
    recognises `drop table` (with lists), `grant` and `revoke`, quoted names, and
    unqualified names by the table's own name. A change that may touch the table is
    treated as one that does.
  - Still heuristics: a quoted name with spaces splits into words, and a search path
    other than the table's schema is not resolved.

- [x] **Collations keep their schema (Codex 9).**
  - A column's collation is a `Collation`: its schema, left out for `pg_catalog`'s, and
    its name. Plans write `collate app.c_copy` rather than `collate c_copy`, which the
    deploy's search path may not find, or may find in another schema.
  - Verify scripts compare the collation with `regcollation`, which resolves the name as
    PostgreSQL does.
  - The new fixture `collation-in-schema` covers it.

- [x] **Owned sequences in another schema (Codex 18, second half): not possible.**
  PostgreSQL refuses `alter sequence … owned by` across schemas ("sequence must be in
  same schema as table it is linked to"), so moving a table aside and back cannot meet
  one. The first half, the global `sqiff_aside.` replace, stays as Phase 4 explains.

- [x] **No stray warnings when a command stops early.** When an error cancels the
  background description, hasql aborts its transactions itself. `withSharedSnapshot` no
  longer rolls them back again on cancellation, which printed "there is no transaction
  in progress" once per connection. It still rolls back on ordinary errors.

- [x] **The table-file rewriter keeps quoted names whole (Codex 20).**
  - `Sqiff.Sql` gains `codeWords` (words split at whitespace and given separators where
    they are code), `parenthesised` and `splitTopLevel`. Quoted names and literals stay
    whole, and comments separate words.
  - The rewriter classifies statements and table elements with them. A column such as
    `"a, b"` or `"x(y)"` used to split into pieces, so retyping or dropping it was left
    to the user ("still differs"). A run against a database showed the old build
    failing and the new one rewriting the file.

- [x] **Table inheritance (Codex 28, in part).**
  - A table records its parents by `inherits`. Columns and constraints record whether
    they are there only because a parent has them.
  - A new child is created with `inherits (…)`, without declaring what it inherits.
    Changed parents become `alter table … inherit` / `no inherit`. Inherited-only
    columns and constraints come and go with the parent's, so they get no steps of
    their own.
  - Before, a child was created as a plain table with copies of its parent's columns,
    and the round trip could not notice. Fixtures `table-inherits` and
    `table-inherit-changed` cover it.

- [x] **Range-type options (Codex 28, in part).**
  - A range type records every option `create type … as range` takes, defaults left out:
    subtype, operator class, collation, canonical and difference functions, and the
    multirange type's name. Before, only the subtype was kept, so a new range type
    lost the rest. `range-type-options` covers it. The `types-added` golden now names
    the multirange type.
  - A changed range type remains a step to write by hand; PostgreSQL cannot alter one.

- [x] **Object owners are left out, by decision (Codex 28, the rest).** Owner roles
  usually differ between environments, and each environment's own role runs its
  migrations. Describing owners would report every object as changed between, say, a
  local database and staging. Grants are compared relative to the owner for the same
  reason.

- [x] **Dependency graph as a type.** Edges no longer travel as `[Edge]` with each user
  building its own lookup. `Sqiff.Dependencies` indexes them once, both ways, and answers
  `dependentsOf`, `referencesOf`, `dependsOn` and `reachableFrom`; `readBoth` returns it,
  and `Planned`, `plan`, `context`, `affected` and the view recreation take it.
  `affected` uses `reachedFrom` instead of its own copy of the search. `schedule` keys
  items by any `Ord` key instead of list positions, so `inPhase` passes step numbers
  directly. `reachableEdges` keeps its progress in a `Reach` record instead of five
  primed variables. Behaviour is unchanged; alga was considered and left out, as
  sqiff's column-aware dependents and its partial schedule on cycles are its own.

- [x] **The reachable-edges query no longer scans pg_depend at every step.** Measuring
  the refactor above found no change (about 1.15 s on a 3,000-table schema, either way)
  but found the real cost: in `depend_from.sql` the planner scanned every user
  `pg_depend` row for each object reached, so the time grew with catalog size times
  view depth. Three chains of 100 nested views over 3,000 tables ran over 10 minutes;
  it now takes about 4.7 s. The lookup is a lateral subquery kept by `offset 0`, so
  it goes through the reference index. A test plans three 100-deep chains over 300
  tables within 3 s: 0.6 s now, 10.3 s before.
- [x] **Recreating views no longer scans the schema per view.** With the query fixed,
  planning was 3.6 s of the 4.7 s on that schema, nearly all in `withViewsAround`:
  `createAgain` walked every definition to find each recreated view's indexes,
  triggers and comments. `attachedIn` groups those once per schema. Planning now
  takes about 120 ms and the whole run about 1.05 s, with the same plan.
- [x] **Two describe-side queries no longer compare everything with everything.**
  PostgreSQL guesses an array parameter holds 10 rows, so in `depend_on.sql` it
  compared each of 3,560 targets with each candidate edge, and checked both ends
  against 28,000 described objects: 443 ms on the server. The caller now keeps the
  edges whose referenced end is a target, and `edgesBetween` already dropped those
  with an undescribed end, so the described list is no longer sent: 28 ms. In
  `describe/column.sql` a join compared each column with every collation; the
  collation is now looked up only for a column whose collation differs from its
  type's: about 290 ms down to 95 ms per half. Reading both databases went from
  887 ms to 532 ms, and the run from 1.05 s to about 0.77 s. What remains is mostly
  `pg_get_viewdef` and `pg_get_constraintdef` on the server, and decoding in a build
  that `cabal.project` compiles without optimisation.

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
  integration harness. The tests find them through `SQIFF_TEST_PG_BIN` and
  `SQIFF_TEST_SQITCH_BIN`, or on PATH.
