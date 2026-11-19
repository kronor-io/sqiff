# sqiff

sqiff compares PostgreSQL schemas and writes the difference as [sqitch](https://sqitch.org) changes.

In a sqitch project, a table's deploy file says what the table *was* when it was first created. Every change after that is a migration someone wrote by hand: an `alter table`, a revert for it, the views that have to be dropped and created again around it, an index that has to be built concurrently in its own change. With sqiff you edit the `create table` file instead, as if you were writing the table today, and sqiff writes the migration that gets an existing database there:

```console
$ git diff deploy/public/table/orders.sql
-    amount integer not null,
+    amount numeric(12,2) not null,
+    status text not null default 'pending',
     created_at timestamptz not null default now()
 );
+
+create index if not exists orders_customer_email on public.orders (customer_email);

$ sqiff table deploy/public/table/orders.sql
-- public.orders.amount
--   dropped and created again around it
--     view public.daily_revenue

-- table column public.orders.amount
-- hazard: rewrites the table and its indexes
-- hazard: holds an ACCESS EXCLUSIVE lock, which blocks reads and writes, while it runs
-- hazard: fails when an existing value does not convert to the new type
-- hazard: recreating a view fails when its query relies on the old type; then edit the view
drop view public.daily_revenue;
alter table public.orders alter column amount type numeric(12,2) using amount::numeric(12,2);
create view public.daily_revenue as
SELECT (created_at)::date AS day,
    sum(amount) AS revenue
   FROM public.orders
  GROUP BY ((created_at)::date);

-- table column public.orders.status
alter table public.orders add column status text not null default 'pending'::text;

-- index public.orders_customer_email
-- outside a transaction
-- hazard: builds an index
do $$ ... $$;   -- stops if an earlier concurrent build left an invalid index
CREATE INDEX CONCURRENTLY orders_customer_email ON public.orders USING btree (customer_email);
```

Add `--write` and sqiff adds these as sqitch changes, with their reverts, and reworks the table's change so that the file you edited stays the one that creates the table.

What sqiff takes care of:

- **Dependents.** It finds the views, functions, constraints and policies that depend on what changes, drops and recreates them in order, and keeps their comments and grants.
- **Hazards.** Every step says what it costs: table rewrites, locks, lost data, casts that can fail, `not null` columns on tables with rows.
- **Safe defaults.** Indexes are built `concurrently`, outside a transaction and in a change of their own. Constraints are validated in their own transaction. Serial columns become identity columns without losing the sequence's value or grants.
- **Renames.** A dropped column and a new one of the same type might be a rename. sqiff asks you which instead of dropping data.
- **Reverts.** Each migration gets a revert script that undoes it, view recreation included.
- **Real catalogs.** sqiff reads both schemas from PostgreSQL itself rather than parsing SQL. Your edit is built in a transaction that is rolled back, so it is never left in the database.

## Contents

1. [Install](#install)
2. [A walkthrough](#a-walkthrough)
   1. [Look at a database](#1-look-at-a-database)
   2. [Compare two databases](#2-compare-two-databases)
   3. [Get the SQL](#3-get-the-sql)
   4. [Write it as sqitch changes](#4-write-it-as-sqitch-changes)
   5. [Change a table by editing its file](#5-change-a-table-by-editing-its-file)
   6. [Renames](#6-renames)
   7. [When the database and the file disagree](#7-when-the-database-and-the-file-disagree)
   8. [Compare with master](#8-compare-with-master)
3. [Commands](#commands)
4. [Settings](#settings)
5. [Tests](#tests)

## Install

Install it from the flake. The repository is private, so nix needs access to it on GitHub, for example a token in `access-tokens` in `nix.conf`.

```bash
nix run github:kronor-io/sqiff -- --help
```

```bash
nix profile install github:kronor-io/sqiff
```

In a clone, `nix build .#sqiff` puts the binary in `result/bin`. `nix develop` gives a shell with GHC, cabal, Postgres, sqitch and git, where `cabal build` and `cabal install` work with the dependency versions in `cabal.project.freeze`.

sqiff needs a reachable Postgres, given with `--db` (`--from` and `--to` for `diff`, `plan` and `sqitch`) or with libpq's `PG*` variables. It also runs `git` and `sqitch`, and `docker` for `--from master`.

The PL/pgSQL body checks need the [plpgsql_check](https://github.com/okbob/plpgsql_check) extension in the database. Without it, sqiff skips them and prints a note on stderr, which is left out of the examples below:

```console
sqiff: plpgsql_check is not installed; PL/pgSQL body dependencies are left out
```

## A walkthrough

The examples use a small shop database. Every output below is what sqiff printed for it.

```sql
create table public.orders (
    id bigint generated always as identity primary key,
    customer_email text not null,
    amount integer not null,
    created_at timestamptz not null default now()
);

create view public.daily_revenue as
    select created_at::date as day, sum(amount) as revenue
    from public.orders group by 1;
```

`shop_prod` has this schema. `shop_dev` has it with three changes: `amount` is `numeric(12,2)`, a `status` column is added, and an index is added on `customer_email`.

### 1. Look at a database

`describe` reads one database and counts its objects by kind:

```console
$ sqiff describe --db "dbname=shop_dev"
1	extension
1	index
3	schema
1	sequence
2	table
5	table column
1	table constraint
2	view
```

With `--out`, it writes one file per object, with grants and comments in files of their own. You can commit the folder and review a schema change as an ordinary git diff:

```console
$ sqiff describe --db "dbname=shop_dev" --out schema
$ find schema -type f | sort
schema/extension/plpgsql.sql
schema/index/public/orders_customer_email.sql
schema/schema/public.comment
schema/schema/public.privileges
schema/schema/public.sql
schema/sequence/public/orders_id_seq.privileges
schema/.sqiff
schema/table%20column/public/orders/amount.sql
schema/table%20column/public/orders/created_at.sql
schema/table%20column/public/orders/customer_email.sql
schema/table%20column/public/orders/id.sql
schema/table%20column/public/orders/status.sql
schema/table%20constraint/public/orders/orders_pkey.sql
schema/table/public/orders.privileges
schema/table/public/orders.sql
schema/view/public/daily_revenue.privileges
schema/view/public/daily_revenue.sql

$ cat "schema/table%20column/public/orders/amount.sql"
type numeric(12,2)
not null
```

`--out` needs an empty or new folder. To refresh a folder that sqiff wrote earlier (the `.sqiff` file marks it), pass `--replace`.

### 2. Compare two databases

`diff` lists what turns `--from` into `--to`. Under each change it lists what depends on the changed object:

```console
$ sqiff diff --from "dbname=shop_prod" --to "dbname=shop_dev"
added   index public.orders_customer_email
changed table column public.orders.amount
  - type integer
  + type numeric(12,2)
  depended on by:
    view public.daily_revenue
added   table column public.orders.status
2 added, 0 dropped, 1 changed
```

It exits with code 1 when the databases differ and 0 when they match, so it can fail a CI job when a database has drifted from the schema in git:

```console
$ sqiff diff --from "dbname=shop_dev" --to "dbname=shop_dev"
0 added, 0 dropped, 0 changed
```

### 3. Get the SQL

`plan` prints the migration. Steps are ordered so that each one can run. Above each step, comments name the object and say how the step runs and what it can do to a live system:

```console
$ sqiff plan --from "dbname=shop_prod" --to "dbname=shop_dev"
-- table column public.orders.amount
-- hazard: may rewrite the table
-- hazard: holds an ACCESS EXCLUSIVE lock, which blocks reads and writes, while it runs
-- hazard: fails when an existing value does not convert to the new type
-- hazard: recreating a view fails when its query relies on the old type; then edit the view
drop view public.daily_revenue;
alter table public.orders alter column amount type numeric(12,2) using amount::numeric(12,2);
create view public.daily_revenue as
SELECT (created_at)::date AS day,
    sum(amount) AS revenue
   FROM public.orders
  GROUP BY ((created_at)::date);

-- table column public.orders.status
alter table public.orders add column status text not null default 'pending'::text;

-- index public.orders_customer_email
-- outside a transaction
-- hazard: builds an index
do $$
begin
    if exists (
        select from pg_index
        where indexrelid = to_regclass('public.orders_customer_email') and not indisvalid
    ) then
        raise exception using message = 'index public.orders_customer_email is invalid from an earlier failed build; drop it first';
    end if;
end
$$;

CREATE INDEX CONCURRENTLY orders_customer_email ON public.orders USING btree (customer_email);
```

Some things to notice:

- `daily_revenue` reads `amount`, so PostgreSQL would refuse to retype the column under it. sqiff drops the view and creates it again around the `alter`, with its comment.
- The index is built `CONCURRENTLY`, so writes to `orders` are not blocked while it builds. That cannot run inside a transaction, so it is marked `outside a transaction`. The `do` block stops a rerun from treating an invalid index left by an earlier failed build as done.
- Other cases have their own recipes. For example, a `serial` column that becomes an identity column keeps the sequence's current value and grants, and a `NOT VALID` constraint that becomes valid is validated in its own transaction. The fixtures in [`test/fixtures`](test/fixtures) show many more: each has `from.sql`, `to.sql` and the `diff` and `plan` sqiff prints for them.

### 4. Write it as sqitch changes

`sqitch` writes the plan into a sqitch project. Run it from the project's root. `--dry-run` lists the changes it would write:

```console
$ sqiff sqitch --from "dbname=shop_prod" --to "dbname=shop_dev" --dry-run
add	migrations/2026/11/2026-11-19-151823-alter-public.orders.amount-status	2 statements	migration: alter public.orders.amount-status
add	public/table/orders/index/orders_customer_email	1 statements	add_index: add index public.orders.orders_customer_email
```

Without `--dry-run`, sqiff calls `sqitch add` (or `sqitch rework`) for each change and fills in its deploy and revert scripts. The index gets a change of its own, because it has to run outside a transaction. sqiff checks the whole batch before writing anything, and if a write fails part-way, it leaves the project as it was. The change names come from the `names` patterns in [`sqiff.yaml`](#settings).

### 5. Change a table by editing its file

In everyday work you usually don't have a second database to compare with. You have the table's deploy file, and you want the table to look different. That is what `sqiff table` is for.

sqiff expects each table to have a deploy file of its own at `deploy/<schema>/table/<name>.sql` (the `names.table` pattern sets this). The file should use `create table if not exists` and `create index if not exists`. When sqiff reworks the change, sqitch runs the new file on databases that already have the table, and there it should do nothing:

```sql
-- deploy/public/table/orders.sql
BEGIN;

create table if not exists public.orders (
    id bigint generated always as identity primary key,
    customer_email text not null,
    amount integer not null,
    created_at timestamptz not null default now()
);

COMMIT;
```

**Step 1: edit the file.** On a branch, change the file to say what the table should be:

```sql
create table if not exists public.orders (
    id bigint generated always as identity primary key,
    customer_email text not null,
    amount numeric(12,2) not null,
    status text not null default 'pending',
    created_at timestamptz not null default now()
);

create index if not exists orders_customer_email on public.orders (customer_email);
```

**Step 2: see the migration.** `sqiff table` reads the file as it is on `master` (or `--base REV`). In one transaction that it rolls back, it builds the table from that version and from your edit, compares the two, and prints what it takes to get from one to the other. `--db` (or `PG*`) only needs to be a database with the project deployed. Nothing in it is changed:

```console
$ sqiff table deploy/public/table/orders.sql
-- public.orders.amount
--   dropped and created again around it
--     view public.daily_revenue

-- table column public.orders.amount
-- hazard: rewrites the table and its indexes
...
```

The output starts with what else the edit touches: here, the view that has to be recreated. When a dependent object would break and sqiff cannot recreate it, the output lists it and `--write` refuses, so that you change or drop the dependent first. With plpgsql_check installed, this also covers PL/pgSQL functions whose bodies use the table.

**Step 3: write it.** Run the same command with `--write`:

```console
$ sqiff table deploy/public/table/orders.sql --write
Created deploy/migrations/2026/11/2026-11-19-151805-alter-public.orders.amount-status.sql
Created revert/migrations/2026/11/2026-11-19-151805-alter-public.orders.amount-status.sql
Created verify/migrations/2026/11/2026-11-19-151805-alter-public.orders.amount-status.sql
Added "migrations/2026/11/2026-11-19-151805-alter-public.orders.amount-status" to ./sqitch.plan
Created deploy/public/table/orders/index/orders_customer_email.sql
...
Tagged "public/table/orders/index/orders_customer_email" with @2026-11-19_sqiff in ./sqitch.plan
Added "public/table/orders [public/table/orders@2026-11-19_sqiff]" to ./sqitch.plan.
...
added migrations/2026/11/2026-11-19-151805-alter-public.orders.amount-status
added public/table/orders/index/orders_customer_email
reworked public/table/orders
```

The plan now ends with:

```text
migrations/2026/11/2026-11-19-151805-alter-public.orders.amount-status   # migration: alter public.orders.amount-status
public/table/orders/index/orders_customer_email                         # add_index: add index public.orders.orders_customer_email
@2026-11-19_sqiff                                                       # sqiff: before the changes it writes
public/table/orders [public/table/orders@2026-11-19_sqiff]              # sqiff: public/table/orders as edited
```

- **The migration** alters existing databases. Its revert drops `status`, changes `amount` back to `integer` and recreates the view around that change too.
- **The index change** builds the index concurrently.
- **The rework of `public/table/orders`** keeps your edited file as the one that creates the table. On a new database it creates the table as it is now. On an existing one, `if not exists` makes it do nothing, because the migrations before it have already done the work.

**Step 4: deploy as usual.**

```console
$ sqitch deploy
Deploying changes to db:pg:shop
  + migrations/2026/11/2026-11-19-151805-alter-public.orders.amount-status .. ok
  + public/table/orders/index/orders_customer_email @2026-11-19_sqiff ....... ok
  + public/table/orders ..................................................... NOTICE:  relation "orders" already exists, skipping
NOTICE:  relation "orders_customer_email" already exists, skipping
ok
```

The two notices are the `if not exists` guards at work: the migrations before the rework have already brought the table up to date.

`sqitch revert` takes the table back to `integer` without `status`.

If you edit the file again on the same branch, run `sqiff table` again. It treats the changes the branch already added for this table as done, and moves the rework after the new migration.

### 6. Renames

From the file alone, a renamed column looks the same as a dropped column and a new column of the same type. sqiff says so, and without your answer it does not write anything that drops data:

```console
$ sqiff table deploy/public/table/orders.sql      # customer_email renamed to email
-- customer_email is dropped and email added with the same type; pass --rename customer_email:email if it is a rename, or --drop-and-add if it is not

-- table column public.orders.customer_email
-- hazard: deletes data
alter table public.orders drop column customer_email;
...

$ sqiff table deploy/public/table/orders.sql --write
sqiff: a column is dropped and another added with the same type; say whether it is a rename with --rename or --drop-and-add

$ sqiff table deploy/public/table/orders.sql --rename customer_email:email
-- table column public.orders.email
-- hazard: breaks running code that still uses the old name
alter table public.orders rename column customer_email to email;
```

Repeat `--rename OLD:NEW` for each renamed column. `rename-hint` in `sqiff.yaml` adds your team's advice to the rename warning, for example how to rename in two deploys.

### 7. When the database and the file disagree

Sometimes a database has drifted from the file: someone added a column by hand, or a migration was never folded back into the file. There are two ways to deal with it.

**Compare with the live table** instead of the file on `master`. sqiff then also shows what is in the database but not in the file. It leaves those things alone unless you pass `--allow-drops`:

```console
$ psql -c "alter table public.orders add column note text"
$ sqiff table deploy/public/table/orders.sql --against-database
-- left alone: table column public.orders.note: in the database but not in the file; --allow-drops drops it
-- the file matches the table
```

**Rewrite the file from the table**, so that the file says what the database already has:

```console
$ sqiff table deploy/public/table/orders.sql --rewrite-file
-- Deploy shop:public/table/orders to pg

BEGIN;

create table if not exists public.orders (
    id bigint generated always as identity primary key,
    customer_email text not null,
    amount integer not null,
    created_at timestamptz not null default now(),
    note text
);

COMMIT;
```

The rewrite keeps the file's text wherever it already matches the table, and changes only what differs. With `--write`, sqiff writes the file and reworks the change. `--live-from CONNINFO` reads the table, read-only, from another database, such as a production replica. sqiff refuses if that database has not yet deployed planned changes that touch the table, because the rewrite would undo them.

### 8. Compare with master

`diff`, `plan` and `sqitch` can build the `--from` side themselves. `--from master`, or `--from rev:REV`, checks out that revision and deploys its sqitch plan into a scratch Postgres started with Docker Compose. sqiff then compares that database with `--to`:

```bash
sqiff diff --from master --to "dbname=app_dev"
```

That answers "what does my dev database have that master's plan does not?". sqiff keeps the scratch database and updates it on later runs. Each repository gets its own, shared by its worktrees and jj workspaces, under `~/.cache/sqiff`. The `baseline` section of [`sqiff.yaml`](#settings) says how to start it.

## Commands

Run every command from the root of the sqitch project, where `sqitch.conf` and `sqiff.yaml` are. `table` and `sqitch` also take `--project DIR`.

| Command | What it does |
| --- | --- |
| `sqiff describe [--db C] [--out DIR [--replace]]` | Count one database's objects, or write one file per object. |
| `sqiff diff --from C\|master\|rev:REV [--to C]` | List what differs and what depends on it. Exits 1 when something differs. |
| `sqiff plan --from … [--to C]` | Print the SQL that turns `--from` into `--to`. |
| `sqiff sqitch --from … [--to C] [--dry-run] [--tag NAME]` | Write that SQL as sqitch changes. |
| `sqiff table FILE [--db C]` | Print the migration for an edited table deploy file. |
| `sqiff table FILE --write` | Add that migration with sqitch and rework the table's change. |
| `sqiff table FILE --rename OLD:NEW` / `--drop-and-add` | Say whether a dropped and added column is a rename. |
| `sqiff table FILE --against-database [--allow-drops]` | Compare with the live table instead of the file at `--base`. |
| `sqiff table FILE --rewrite-file [--live-from C] [--write]` | Rewrite the file to create the table as the database has it. |

`diff`, `plan`, `sqitch` and `describe` also take `--exclude-schema SCHEMA` (repeatable), `--include-partitions` and `--no-body-check`. `sqiff COMMAND --help` lists every option.

Exit codes: 0 when the command succeeds, 1 when `diff` finds differences (or `--rewrite-file` leaves some), and 2 when there is an error, with a one-line `sqiff: …` message on stderr.

## Settings

**Databases.** `--db`, `--from` and `--to` take a libpq connection string. When `--db` or `--to` is left out, libpq's environment names the database: `PGHOST`, `PGPORT`, `PGUSER`, `PGDATABASE`, or `PGSERVICE` with a service file.

**sqitch layout.** The top directory and plan file come from `sqitch config` (`core.top_dir`, `core.plan_file`); `--top-dir` and `--plan-file` override them.

**`sqiff.yaml`**, next to `sqitch.conf`, holds the rest. Every key is optional; an unknown key is an error.

```yaml
exclude-schemas: [audit]             # left out of every comparison
rename-hint: "..."                   # added to the warning on a column rename
templates: sqitch/templates          # sqitch templates for new changes: add_column, add_index, validate_constraint, migration

names:                               # change names; the defaults are shown
  table: "{schema}/table/{name}"
  function: "{schema}/function/{name}"
  view: "{schema}/view/{name}"
  trigger: "{schema}/trigger/{table}/{name}"
  index: "{schema}/table/{table}/index/{name}"
  type: "{schema}/type/{name}"
  migration: "migrations/{stamp}-{slug}"
  migration-stamp: "%Y/%m/%Y-%m-%d-%H%M%S"

baseline:                            # the scratch database --from master builds
  compose-file: docker-compose.yaml  # read from the revision's checkout
  service: postgres
  port-variable: PG_PORT             # the compose file's variable for the published port
  port: 0                            # 0 lets Docker pick a free port
  container-port: 5432
  environment: {}                    # more variables for docker compose
  user: postgres
  database: postgres
  password-variable: PG_PASSWORD     # or password: ...; left out, trust auth, PGPASSWORD or ~/.pgpass
  sqitch: host                       # host, or service to run sqitch inside the service
  deploy:                            # sqitch calls that deploy the revision
    - sqitch: [deploy]
      if-exists: sqitch.plan         # skip the call when this path is missing
  status: [status]                   # the sqitch call that finds changes the plan no longer has
```

In `deploy` and `status` arguments, `{host}`, `{port}`, `{user}` and `{database}` are where that sqitch reaches the baseline: the port Docker published when sqitch runs on the host, `container-port` when it runs in the service. sqitch reads `PGHOST` and `PGPORT` only for what a target's URI leaves out, so a target with a fixed URI needs one, for example:

```yaml
  deploy:
    - sqitch: [deploy, "db:pg://{user}@{host}:{port}/{database}"]
  status: [status, "db:pg://{user}@{host}:{port}/{database}"]
```

The password reaches sqitch as `PGPASSWORD` and sqiff's own connections in the connection string; it is never put in sqitch's arguments.

With `templates`, `--write` adds a change that adds a column, adds an index or validates a constraint from the matching template, passing `schema`, `table_name` and `column_name`, `index_name` or `constraint_name`; other new migrations use `migration`. In `add_column`, sqiff puts the column's type and options where `__your_type_here__ null` is, and it expects `add_index` to name the index `idx_<table_name>_<index_name>`. create-migration's templates already fit, so `templates` can point at them. Without `templates`, `--write` uses sqitch's own templates and writes the scripts itself.

## Tests

```bash
cabal test
```

The unit tests are pure. The integration tests start a throwaway PostgreSQL cluster (`initdb` from `PATH`, or the directory in `SQIFF_TEST_PG_BIN`), and for each directory under `test/fixtures` they build `from.sql` and `to.sql` and compare `sqiff diff` and `sqiff plan` with the golden files. They then apply the plan to `from` and expect `diff` to find nothing left, and do the same through `sqitch deploy`, verify and revert (`sqitch` from `PATH`, or the directory in `SQIFF_TEST_SQITCH_BIN`). A `known-bug` file marks a fixture that fails today, and a `revert-gap` file one whose changes PostgreSQL cannot fully revert; each says why. Refresh golden files with `cabal test --test-options=--accept`.
