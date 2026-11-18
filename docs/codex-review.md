I reviewed all 15 modules in `src/`, `app/Sqiff.hs`, and the catalog queries they consume. **The main risk is semantic correctness: several paths can generate migrations that fail, silently change SQL expressions, or reproduce the wrong permissions.** Splitting the large modules will help, but preserving structure instead of repeatedly parsing rendered SQL should come first.

This is a static review. I did not run the application: the available GHC lacks its dependencies, and PostgreSQL/Sqitch are not on PATH. There is also no test suite declared in `sqiff.cabal`. No files were changed.

Below, **P1** means migration failure, silent schema differences, or incorrect permissions; **P2** means a narrower correctness or operational issue.

The correctness findings are:

1. **P1 — Object ranks override actual dependencies.**  
   [Plan.hs:29](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Plan.hs:29) sorts by `(phase, rank, dependencyPosition, ...)`, while [Recipe.hs:137](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:137) assigns fixed ranks by object kind. Consequently, a function needing a newly created view is placed before that view; a new table whose default calls a new function is placed before the function. The dependency graph cannot correct either order. Folding column definitions into table creation at [Recipe.hs:255](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:255) also means the resulting table step must inherit those columns’ dependencies. Build a graph of **operations**, with rank used only to choose between ready nodes.

2. **P1 — Cycles produce apparently executable plans instead of planning errors.**  
   [Plan.hs:124](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Plan.hs:124) appends all remaining nodes in set order when topological sorting stalls. This includes cycles and nodes blocked by cycles. The caller receives neither a diagnostic nor a manual-resolution requirement. Return `Either (NonEmpty DependencyCycle) [Operation]`; explicitly handle any cycles that can be broken using staged creation.

3. **P1 — Dependency reporting can hide objects that still block a migration.**  
   [Diff.hs:55](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Diff.hs:55) excludes an address from `affected` if **any aspect** changed. Changing a view’s comment therefore hides that view as a dependent of a retyped column, even though its definition remains untouched. Additionally, `affected` implements its own traversal rather than using the column-aware [Diff.hs:103](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Diff.hs:103). Use one normalized dependency graph, and suppress dependents only when the plan actually handles their dependency—not merely because their address appears in the diff.

4. **P1 — ACL normalization loses security-relevant distinctions.**  
   [privileges.sql:26](/home/lorenzo/Projects/Public/sqiff/sql/describe/privileges.sql:26) maps both a null ACL and an explicit empty ACL to `"default"`. These are different states. Meanwhile, [Recipe.hs:600](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:600) treats an added object’s initial privileges as empty and only grants the desired entries. A newly created function can therefore retain default `PUBLIC EXECUTE` even when the desired function revoked it. PostgreSQL’s defaults can grant privileges to `PUBLIC`; they must be accounted for explicitly. [PostgreSQL default privileges](https://www.postgresql.org/docs/18/sql-alterdefaultprivileges.html)  
   Decode ACL entries structurally, distinguish defaults from explicit ACLs, and reconcile against the privileges creation actually produces.

5. **P1 — ACL parsing fails for legal role names and mishandles grant-option changes.**  
   [privileges.sql:31](/home/lorenzo/Projects/Public/sqiff/sql/describe/privileges.sql:31) emits raw role names separated from privileges by a space; [Recipe.hs:622](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:622) splits at the first space. Role `"report readers"` becomes role `report` and privilege text `readers SELECT`. Newlines are similarly ambiguous. Grant-option removal at [Recipe.hs:617](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:617) revokes the entire privilege and regrants it, rather than issuing `REVOKE GRANT OPTION FOR`; downstream grants can make this fail. Preserve grantee, privilege, grant option, and grantor as separate fields.

6. **P1 — Formatting and replay transformations can change quoted SQL content.**  
   [Sqitch.hs:390](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:390) globally replaces keywords in index expressions. An expression containing the literal `' WHERE '` becomes `' where '`. The foreign-key formatter at [Sqitch.hs:355](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:355) can similarly alter quoted identifiers containing keyword fragments. [app/Sqiff.hs:1108](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:1108) removes `concurrently ` anywhere in replayed statements, including literals and function bodies. Formatting and replay adaptation must operate on tokens outside quoted regions, or on typed operations.

7. **P1 — The column-description format cannot represent arbitrary SQL expressions.**  
   [column.sql:60](/home/lorenzo/Projects/Public/sqiff/sql/describe/column.sql:60) embeds expression text into a newline-delimited format. [Recipe.hs:679](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:679) then parses it with `T.lines`, ignores unknown lines, and accepts an empty type. A multiline string default or generated expression can be truncated; a continuation resembling `type ...` can overwrite another field. Return separate typed columns or JSON from the catalog query. As an interim measure, make parsing return `Either` and reject unconsumed or duplicate fields.

8. **P1 — Fixed dollar-quote delimiters do not safely contain interpolated names.**  
   Examples include [Recipe.hs:81](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:81), [Recipe.hs:349](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:349), [Recipe.hs:782](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:782), and [Sqitch.hs:469](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:469). An identifier containing `$$` terminates the outer body even when correctly escaped as an inner SQL literal. Choose a delimiter absent from the entire body. PostgreSQL explicitly requires distinct delimiters where nesting would conflict. [Dollar quoting](https://www.postgresql.org/docs/18/sql-syntax-lexical.html)  
   Separately, `concurrentBuild` interpolates an index name into a `RAISE` format string but escapes only apostrophes. A name containing `%` changes the format’s parameter requirements. Use `RAISE ... USING MESSAGE = ...`. [RAISE formatting](https://www.postgresql.org/docs/18/plpgsql-errors-and-messages.html)

9. **P1 — Literal and collation handling is not fully round-trippable.**  
   [column.sql:9](/home/lorenzo/Projects/Public/sqiff/sql/describe/column.sql:9) records only a collation’s name, losing its schema. A custom collation can fail to resolve or resolve differently during deployment. [Recipe.hs:746](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:746) parses enum labels with a handwritten quoted-literal parser that does not handle PostgreSQL `E'...'` literals; a backslash-bearing label containing `, ` can be split incorrectly. Also, the duplicated literal renderers assume standard-conforming strings without establishing that session setting. Preserve enum values as data and render literals through one explicit policy.

10. **P1 — Drop/recreate paths lose unchanged metadata.**  
    Materialized-view replacement restores indexes and privileges at [Recipe.hs:558](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:558), but not unchanged comments on the view, its columns, or recreated indexes. Concurrent index replacement at [Recipe.hs:463](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:463) likewise drops the old index’s unchanged comment. The schema diff contains no comment change, so no later step restores it. Every destructive recreation needs an explicit inventory of properties and child objects to restore.

11. **P1 — Serial/identity conversion conflicts with independent sequence planning.**  
    [Recipe.hs:348](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:348) embeds sequence removal/creation inside the column conversion. The same sequence can independently appear as `Dropped` or `Added` and be processed by [Recipe.hs:486](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:486). For serial → identity, the drop phase can attempt to remove the sequence while the old default still depends on it. The opposite direction can create the sequence twice. The conversion also synthesizes a sequence name rather than preserving the desired serial sequence’s identity, and its ACL restoration loses grant options. Model this as one coordinated operation consuming both changes.

12. **P1 — View-option removal is not implemented.**  
    [Recipe.hs:549](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:549) uses `CREATE OR REPLACE VIEW` with the new options, but generates no `ALTER VIEW ... RESET (...)` for options removed from the desired definition. This can leave settings such as `security_barrier` enabled after a supposedly successful migration. Compute option additions, changes, and removals separately.

13. **P1 — Sqitch layout overrides are read but not passed to Sqitch writes.**  
    [app/Sqiff.hs:540](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:540) honors `--plan-file` and `--top-dir` when reading and constructing paths. But [Sqitch.hs:497](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:497) does not even receive the selected plan path, and [Sqitch.hs:571](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:571) invokes Sqitch without layout overrides. It can modify the configured default plan while sqiff reads or overwrites another location. Custom script directories and extensions are also ignored by the hardcoded path at line 541. Resolve one complete layout and use it for every read, subprocess invocation, and write. [Sqitch configuration](https://sqitch.org/docs/manual/sqitch-configuration/)

14. **P1 — Generated change identities collide, without preflight validation.**  
    [Sqitch.hs:229](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:229) gives all overloads of a routine the same conventional name. `units` consults only the initial plan, so two new overloads can both become `AddChange` with the same name. [app/Sqiff.hs:1229](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:1229) creates a tag unique only by date, so a second rework that day can attempt to add an existing tag. Raw schema/object names also flow directly into change paths without a validated path representation. Validate the entire batch’s names, collisions, path containment, and tag availability before the first mutation.

15. **P1 — Writes can leave an inconsistent project after failure.**  
    [Sqitch.hs:503](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:503) mutates the plan via Sqitch before overwriting scripts. A later process error, missing template placeholder, or file-write failure leaves partial results. [app/Sqiff.hs:688](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:688) overwrites the deploy file before its rework succeeds. The existing error message acknowledges partial writes, but there is no recovery mechanism. Prepare and validate an artifact batch in an isolated staging project, then install it with backups or a recoverable journal.

16. **P2 — Reverts describe whole object changes, not individual migration units.**  
    [Sqitch.hs:160](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:160) assigns the entire inverse recipe to the first unit that claims an object key; later units get no inverse. For a unique constraint split into concurrent index creation and attachment, reverting only the attachment unit does nothing. That violates the expectation that each change’s revert undoes that change. Either keep the operation together with documented transaction boundaries or derive inverses for explicit intermediate states.

17. **P1 — Drift normalization can erase genuine differences.**  
    [Rewrite.hs:42](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Rewrite.hs:42) removes `" not valid"` and `" NOT VALID"` globally, regardless of object aspect or quoting. Comments `"status not valid"` and `"status"` compare equal; defaults and expressions can also be affected. The same normalization is used by the post-rewrite check, so that check cannot catch this class of error. Normalize only a constraint’s validation field.

18. **P1 — Moving tables aside and restoring names rewrites expression contents.**  
    [TableEdit.hs:313](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/TableEdit.hs:313) globally replaces `sqiff_aside.` in every definition and comment. A literal containing that text is changed even though it is not an object reference. Owned-sequence discovery at [TableEdit.hs:290](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/TableEdit.hs:290) returns only the sequence name, while line 226 assumes it belongs to the table’s schema. Cross-schema owned sequences break that assumption. Keep catalog identities and qualified sequence names, and remap structured references rather than text.

19. **P2 — Transaction filtering changes valid savepoint semantics.**  
    [Script.hs:108](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Script.hs:108) preserves `ROLLBACK TO ...`, but drops `ROLLBACK WORK TO ...` and `ROLLBACK TRANSACTION TO ...`. Both are valid PostgreSQL forms. [ROLLBACK TO syntax](https://www.postgresql.org/docs/18/sql-rollback-to.html)  
    A script can therefore retain DDL that its author intended to undo. The classifier needs token-aware parsing of transaction-control statements. The claim that savepoints stay is currently too broad.

20. **P2 — Several SQL parsers fail on quoted names containing punctuation or whitespace.**  
    [app/Sqiff.hs:316](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:316) splits foreign-key targets on the first dot, parenthesis, and comma, and does not unescape doubled quotes. A valid reference to `"a.b"."t"` can silently skip compatibility checks. [Rewrite.hs:233](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Rewrite.hs:233) uses `T.words` for statement classification, and [Rewrite.hs:477](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Rewrite.hs:477) treats punctuation inside quoted column names as separators. Unsupported syntax should produce a diagnostic, not quietly bypass analysis.

21. **P1 — Branch replay and undeployed-change checks depend on unreliable text/name heuristics.**  
    [app/Sqiff.hs:1096](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:1096) recognizes table migrations using the original case-sensitive table name, but [Sqitch.hs:310](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:310) lowercases and sanitizes migration slugs. Mixed-case or punctuation-bearing names can make sqiff overlook its own earlier migrations. [app/Sqiff.hs:720](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:720) also misses `DROP TABLE` and cannot reliably resolve unqualified names/search paths. These heuristics are used to decide whether rewriting would undo pending work. Record touched object identities explicitly; treat unclassifiable pending scripts conservatively.

22. **P1 — Baseline state is shared without locking and cached too loosely.**  
    [Baseline.hs:42](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Baseline.hs:42) shares one checkout, database, and state file across worktrees. Concurrent calls for different revisions can race checkout, deploy, teardown, and recording the commit. The cache-hit condition at line 51 checks only commit and container-running status, ignoring configuration changes and database drift. [Baseline.hs:150](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Baseline.hs:150) ignores the status exit code and recognizes stale state through the English substring `"Cannot find"`. Add a repository-scoped lock, fingerprint relevant configuration, and distinguish status failure from known deployment state.

23. **P2 — Connection and worker lifetimes are unmanaged.**  
    [app/Sqiff.hs:358](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:358), [app/Sqiff.hs:480](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:480), and the table workflows acquire connections without releasing them. Successful acquisitions also leak if another concurrent acquisition fails. Detached `async` work at lines 360, 370, and 615 is not scoped to the caller. `connectScratch` leaks its acquired connection if validation fails. Use bracketed acquisition—including partial pool acquisition—and `withAsync`. The CLI process exiting eventually limits the leak, but does not establish safe library semantics.

24. **P2 — Cleanup and probing errors are sometimes silently converted to absence.**  
    [Catalog.hs:405](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Catalog.hs:405) discards temp-table setup results; [TableEdit.hs:146](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/TableEdit.hs:146) ignores savepoint creation and rollback failures; [TableEdit.hs:99](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/TableEdit.hs:99) collapses all type-probe failures into `Nothing`. [Body.hs:73](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Body.hs:73) handles returned SQL errors but does not scope rollback around exceptions interrupting the session. Distinguish “unsupported probe” from connection failure, and preserve both the primary error and any cleanup error.

25. **P2 — Catalog reads do not form a consistent snapshot.**  
    [Catalog.hs:169](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Catalog.hs:169) runs queries across independent connections and transactions, even splitting some queries by OID parity. Dependencies and body checks run later. Concurrent DDL can produce a schema assembled from incompatible states. Use an exported snapshot for parallel read transactions, or detect changes and retry; define separately how mutation-based body checks relate to that snapshot.

26. **P1/P2 — Folder export can overwrite objects and fails on empty output.**  
    [Folder.hs:74](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Folder.hs:74) maps both `a b` and `a-b` to the same segment, and both `/` and literal `%2F` to `%2F`. Distinct objects silently overwrite each other. At [Folder.hs:36](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Folder.hs:36), the root is created only while writing objects; an empty schema exported to a new or replaced directory fails when writing the marker. Use an injective encoding, detect collisions, create the root explicitly, and stage replacement before deleting the old export.

27. **P2 — SQL verification can report success without verifying the intended outcome.**  
    [Sqitch.hs:435](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:435) has no checks for tables, routines, schemas, extensions, or types; conventional changes receive an empty verification set at line 509. Type verification compares only `atttypid`, so changing `varchar(10)` to `varchar(20)` is not verified. Manual-only recipes at [Recipe.hs:209](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:209) can still become successful no-op migrations. Represent unsupported operations explicitly and prevent normal writing of an incomplete plan.

28. **P2 — The schema model omits important observable properties.**  
    Column order is discarded by the schema map and reconstructed in map order at [Plan.hs:86](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Plan.hs:86). New tables can therefore have a different `SELECT *`/positional-insert order. The catalog descriptions also omit ownership, ordinary inheritance, various range-type options, and other properties: see [relation.sql:13](/home/lorenzo/Projects/Public/sqiff/sql/describe/relation.sql:13) and [type.sql:32](/home/lorenzo/Projects/Public/sqiff/sql/describe/type.sql:32). Define the supported equivalence explicitly and report unsupported properties that affect a planned recreation.

29. **P2 — Some diagnostic and process inputs need dedicated boundaries.**  
    [app/Sqiff.hs:353](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:353) returns raw connection strings for error messages, potentially printing passwords. Use a redacted display representation. [Git.hs:65](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Git.hs:65) accepts raw revisions without an option terminator; resolve a validated revision once and use the resulting commit identity downstream. The use of `typed-process` with argument lists is good: these calls are not shell-string interpolation vulnerabilities.

I did **not** find widespread unchecked `head`, `fromJust`, `read`, or `!!` usage. The more significant partiality is semantic: parsers manufacture defaults or skip malformed input rather than rejecting it. The `RIO.Text.Partial` imports alone are not evidence of a runtime crash; their actual arguments need examining.

Architecturally, the existing pure `Diff`, `Object`, and much of `Recipe` are useful foundations. The central problem is that **rendered text serves simultaneously as catalog serialization, comparison representation, migration operation, and presentation format**.

Every module’s boundary would benefit from the following changes:

| Module | Assessment and concrete direction |
|---|---|
| [app/Sqiff.hs:85](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:85) | Contains dependency discovery, concurrent loading, body analysis, planning policy, branch replay, rendering, writing, and CLI parsing. Move workflows into library modules; leave parsing, environment construction, and exit handling in `app/`. |
| [Recipe.hs:181](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:181) | One roughly 400-line function contains almost every object recipe. Separate column, constraint, index, routine, relation, and privilege planning. Keep operation rendering separate from recipe selection. |
| [Catalog.hs:123](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Catalog.hs:123) | Owns domain types, application errors, connections, SQL codecs, parallel scheduling, dependency queries, and body analysis. Split domain model, database access, and body-analysis services. |
| [Sqitch.hs:137](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:137) | Combines plan parsing, unit planning, naming, SQL formatting, verification, filesystem writes, and subprocesses. Separate pure artifact preparation from installation. |
| [Rewrite.hs:63](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Rewrite.hs:63) | Pure logic is a strength. Separate lexical analysis, source edits, and drift policy; return source spans so reconstruction does not search for statement text again. |
| [TableEdit.hs:210](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/TableEdit.hs:210) | Mixes transaction orchestration, scratch setup, snapshot normalization, diffing, and ignore policy. Extract the pure comparison policy and keep database mutation in one scoped interpreter. |
| [Plan.hs:28](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Plan.hs:28) | Separate context construction, operation graph construction, scheduling, and rendering. Cycles must be represented in the result. |
| [Diff.hs:45](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Diff.hs:45) | The map merge is appropriately small and pure. Consolidate its multiple reachability implementations around one graph abstraction. |
| [Object.hs:25](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Object.hs:25) | A good ADT foundation, but identity is duplicated with `Address`. Move both into a database-independent model and make conversion explicit and checked. |
| [Body.hs:106](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Body.hs:106) | Separate database round-trip checking from definition equivalence and repair. The global `excluded_1`…`excluded_9` replacement is a narrow heuristic; retain its successful-round-trip guard and document its limits. |
| [Config.hs:185](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Config.hs:185) | YAML decoding, naming-template parsing, and external Sqitch layout discovery are distinct responsibilities. Parse configuration into validated domain values before constructing workflows. |
| [Baseline.hs:37](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Baseline.hs:37) | This is an effectful service, appropriately so. Give it an explicit state machine, lock, cache identity, and structured process outcomes. |
| [Git.hs:19](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Git.hs:19) | A reasonable adapter boundary. Strengthen repository paths and revision/commit types; centralize process-error handling. |
| [Folder.hs:27](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Folder.hs:27) | Split pure `Schema -> ExportManifest` from staged filesystem installation. The manifest should establish unique paths before writing. |
| [Script.hs:21](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Script.hs:21) | Keep the explicit lexical state machine, but share it with rewrite/classification code. Return tokens/spans and classify transaction control separately. |
| [Timing.hs:18](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Timing.hs:18) | Small and focused. Read timing configuration once in the environment. Document that `measure` forces an `Int` proxy, not an arbitrary result to normal form, and that failed actions currently emit no completion timing. |

There is already some RIO usage, but it starts late in the table workflow. [TableEnv at app/Sqiff.hs:550](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:550) is principally a bag of inputs; it does not own connection lifetimes, process capabilities, logging, or resource scopes. Meanwhile, data records embed effects—`PlFunctions.tempTables` at [Catalog.hs:350](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Catalog.hs:350), `Dependents.dbTempTables`, and positional `Connected`. This obscures whether an action is cached, repeatable, or tied to an open connection.

A practical direction is an `AppEnv` containing validated configuration, logging/timing, and narrow database/Git/Sqitch services. Keep planning pure. Use capability records initially; introduce `HasLogFunc`/other RIO-style capabilities where sharing warrants them. A `MonadCatalog` class can help if both production and test interpreters are useful, but a broad class covering every operation would simply move the current coupling.

For data types, these are the highest-value changes:

```haskell
-- Constructors hidden; external inputs go through smart constructors.
newtype Identifier = Identifier Text
newtype ChangeName = ChangeName Text
newtype Revision = Revision Text
newtype CommitId = CommitId Text
newtype ConnectionString = ConnectionString Text

data QName = QName
  { schema :: Identifier
  , name   :: Identifier
  }

data CatalogId = CatalogId
  { classId    :: ClassId
  , objectId   :: ObjectId
  , subobjectId :: SubobjectId
  }

data Grantee = Public | Role Identifier
data GrantOption = WithoutGrantOption | WithGrantOption
data Acl = DefaultAcl | ExplicitAcl (Set Grant)
```

`ConnectionString` should have an explicit redacted display function, not a revealing derived `Show`. `ChangeName` needs a path-safe representation and Sqitch grammar validation. `Revision` and resolved `CommitId` are different states, not interchangeable newtype labels.

The most important “parse, don’t validate” opportunities are:

- **Catalog address decoding:** [Catalog.hs:628](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Catalog.hs:628) accepts arbitrary type/name/argument lists, followed by repeated pattern matching throughout the program. Decode once into known object identities or an explicit unsupported-object value.
- **Column descriptions:** `parseColumn :: Text -> ColumnDef` cannot report failure and permits contradictory states.
- **Plan parsing:** [Sqitch.hs:90](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Sqitch.hs:90) manufactures a plan with an empty project when missing, and independently re-parses entries in `plannedChanges` and `moveToEnd`. Parse a structured plan once, preserving dependencies, tags, timestamps, and source positions.
- **Naming templates:** [Config.hs:198](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Config.hs:198) substitutes sequentially, so inserted placeholder-looking text can be expanded again. `match` separately interprets the template and accepts malformed braces. Parse a `NameTemplate` AST once and validate required/allowed placeholders.
- **Table commands:** incompatible combinations of mode, comparison, renames, and live source survive CLI parsing and are rejected much later at [app/Sqiff.hs:638](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:638) and [app/Sqiff.hs:787](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:787). Construct a validated command before connecting or starting workers.
- **File locations:** `locateTable` returns a raw `(FilePath, Text, QName)` after checking conventions. Return a `TableDeployFile` containing a validated project-relative path and change identity.
- **Connections:** `requireScratch` and `readOnly` establish capabilities, then return/use the same raw connection type. Scoped `ScratchSession` and `ReadOnlySession` wrappers would make the established state visible to callers.
- **Nonempty pools:** `describeOn` takes a list and silently returns empty maps for `[]`; use `NonEmpty Connection`.

`ColumnDef` is the clearest example of invalid states being representable:

```haskell
data Nullability
  = Nullable
  | NotNull ConstraintNamePolicy ValidationState

data ColumnValue
  = PlainColumn (Maybe SqlExpr)
  | IdentityColumn IdentityGeneration IdentityOptions
  | GeneratedColumn StorageKind SqlExpr

data ColumnDef = ColumnDef
  { columnType :: TypeRef
  , collation  :: Maybe QName
  , nullability :: Nullability
  , value      :: ColumnValue
  }
```

This replaces the independent `notNull`, `notNullValid`, `notNullName`, `identity`, `identityOptions`, `generated`, and `defaultExpr` fields at [Recipe.hs:664](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:664). `rewritesOnAdd` belongs in analysis evidence or operation hazards rather than the semantic column definition.

Other worthwhile records are `RetypedColumn` instead of the four-tuple at [app/Sqiff.hs:566](/home/lorenzo/Projects/Public/sqiff/app/Sqiff.hs:566), `RecreationBundle` instead of `([Text], Maybe (ObjectKey, Text))`, and `AnalysisResult` instead of `Maybe` plus textual `"not checked: "` sentinels. Existing ADTs such as `Txn`, `Drops`, `PreviousFile`, and `BodyCheck` are good examples to extend; ordinary factual Booleans need not all become custom types.

A typed diff/render pipeline would remove much of the ad-hoc inspection:

```haskell
data Change a
  = Added a
  | Dropped a
  | Changed a a

data Operation
  = AddColumn ColumnId ColumnDef
  | AlterColumnType ColumnId TypeRef SqlExpr
  | BuildIndex IndexDef BuildMode
  | AttachConstraint TableId ConstraintDef
  | SetPrivileges ObjectId Acl
  | RecreateObject RecreationBundle

data PlannedItem
  = Executable Operation
  | NeedsManualWork ObjectId ManualReason

planMigration
  :: SchemaSnapshot
  -> SchemaSnapshot
  -> Either (NonEmpty PlanError) MigrationPlan

renderMigration
  :: RenderOptions
  -> MigrationPlan
  -> MigrationArtifacts
```

Keep SQL expressions opaque where a full SQL AST is unnecessary, but distinguish them from identifiers, literals, statements, and whole scripts. Derive transaction requirements, hazards, touched objects, verification, and inverse operations from `Operation`. This replaces SQL substring checks in `shapeOf`, `verb`, `gone`, `replayable`, and `conventionalName`.

`Step` currently duplicates `address`, `object`, and `key.address` at [Recipe.hs:92](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Recipe.hs:92). All constructors are exported, so inconsistency is possible by construction. Store one identity, derive the others, and hide the constructor.

On readability and duplication:

- The largest functions are `definition`, `findingsFor`, `reachableEdges`, `editTable`, and `rewriteFile`. Split them around intermediate results with named types, rather than extracting numerous helpers that still capture the same large closure.
- SQL literal quoting is duplicated in Recipe, Catalog, Sqitch, and Rewrite. SQL lexing overlaps between Script and Rewrite; comment stripping appears again in TableEdit and the application.
- Index-owner parsing appears in Recipe, Sqitch, TableEdit, Plan, and application view reconstruction. Catalog data should provide that relationship directly.
- `describe`, `dependencies`, and some other exported helpers have no in-repository callers. They may be intentional library API; audit before removing. More concretely, `Rewrite.unplaced` is computed but never consumed by the application, which instead performs its own remaining-drift reporting.
- [Rewrite.hs:140](/home/lorenzo/Projects/Public/sqiff/src/Sqiff/Rewrite.hs:140) returns `Address "" [] []` for unmatched objects, including `Table` values produced elsewhere in that workflow. A total conversion or explicit failure is clearer than a fabricated identity.
- `drop'` in enum planning actually finds a preceding label; `plain`, `made`, `same`, `with`, and similar short names become hard to follow in large scopes. Name the represented state.
- Project-specific comments mentioning “kronor” and “create-migration” should identify an optional integration rather than appearing to define generic Sqitch behavior.
- The extensive export of constructors and implementation helpers in [sqiff.cabal:25](/home/lorenzo/Projects/Public/sqiff/sqiff.cabal:25) makes later invariant enforcement harder. Establish a smaller public API.

I would ship the refactoring in these phases, each with a concrete deliverable:

1. **Stop silent corruption and permission mistakes.**  
   Add regression fixtures for ACL defaults/empty ACLs, quoted role names, multiline defaults, literal-preserving formatting, dollar delimiters, drift normalization, and folder collisions. Fix these narrowly; reject unsupported cases explicitly. This phase can ship without reorganizing the application.

2. **Make planning dependency-correct.**  
   Introduce operation dependencies, report cycles, carry folded column dependencies into table creation, fix `affected`, and coordinate serial/identity changes. Test cross-kind dependencies and compare the deployed result with the desired catalog snapshot.

3. **Make Sqitch output coherent and recoverable.**  
   Resolve a complete layout, validate unique names and tags, stage all artifacts, and add recovery for installation failures. Define correct revert behavior at each generated change boundary. Test custom layouts, overloaded routines, repeated same-day runs, and injected write/process failures.

4. **Introduce a typed catalog model incrementally.**  
   Start with columns and ACLs, then indexes, constraints, relations, and routines. Decode structured query results; retain a compatibility renderer for existing descriptions. Add identifier, change-name, revision, commit, and path types with hidden constructors.

5. **Extract workflows and scope effects.**  
   Move CLI-independent behavior out of `app/Sqiff.hs`; split Catalog and Sqitch; introduce the RIO environment and bracketed sessions/workers. Add baseline locking and a stronger cache identity. Verify cleanup on failure and simultaneous baseline requests.

6. **Consolidate parsing and retire heuristics.**  
   Share one lexer with source spans, replace SQL substring classification with operations or parsed syntax, and make unsupported rewrite syntax explicit. Expand round-trip integration tests to comments, privileges, view options, column order, and recreated dependencies. Remove duplicate parsers and unused API only after callers have migrated.