# sqiff

sqiff compares PostgreSQL schemas and writes the difference as sqitch changes, for kronor's sqitch project.

## Commands

- `sqiff table FILE --db CONNINFO [--write]` prints the migration for an edited table deploy file. It builds the table from the file at `--base` (master) and from your edit inside one rolled-back transaction. With `--write` it adds the migration with `sqitch add` and reworks the table's change.
- `sqiff table FILE --db CONNINFO --rewrite-file [--live-from CONNINFO] [--write]` rewrites a drifting table file from the live table.
- `sqiff describe`, `diff`, `plan` and `sqitch` read whole databases. `--from master` builds a scratch database from master's sqitch plan with Docker Compose.

Run it from the root of a kronor checkout. `--write` uses kronor's templates in `_dev/sqitch/templates`, and `--from master` uses the checkout's `docker-compose.yaml`.

## Build

```bash
nix build .#sqiff
```

Inside kronor's dev shell, `cabal build` works too: the shell's GHC already has every dependency at the versions in `cabal.project.freeze`.

## Needs at run time

`git`, `sqitch`, and `docker` for `--from master`. The database needs plpgsql_check for the PL/pgSQL body checks; without it they are skipped with a note.
