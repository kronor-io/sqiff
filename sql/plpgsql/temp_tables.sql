-- Creates, in this session, the temp tables PL/pgSQL functions create, so plpgsql_check
-- can analyse the statements that use them. plpgsql_make_pragma describes the ones a
-- function's own statements create, when it can plan their queries; a query on a temp
-- table another statement or a caller creates plans once that table exists, so this
-- repeats until no new table appears. It warns, whatever client_min_messages says, for
-- each table that already exists, so a function is described only while none of the
-- tables it creates do. The statements that made them are appended to the setting
-- sqiff.temp_tables. @extschema@ is replaced with plpgsql_check's schema; before
-- 2.10 this does nothing.
set client_min_messages = error;
do $$
declare
    f regprocedure;
    created text[];
    pragma text;
    name text;
    ddl text;
    added int;
    rounds int := 0;
begin
    if to_regprocedure('@extschema@.plpgsql_make_pragma(regprocedure, regclass, boolean)') is null then
        return;
    end if;
    loop
        added := 0;
        rounds := rounds + 1;
        for f, created in
            select p.oid::regprocedure, array(
                select t.m[1]
                from regexp_matches(p.prosrc, 'create\s+(?:temp|temporary)\s+table\s+(?:if\s+not\s+exists\s+)?([a-z_][a-z0-9_$]*)', 'gi') t(m)
            )
            from pg_proc p
            join pg_language l on l.oid = p.prolang
            join pg_namespace n on n.oid = p.pronamespace
            where l.lanname = 'plpgsql'
                and p.prorettype <> 'trigger'::regtype
                and n.nspname <> 'information_schema'
                and n.nspname !~ '^pg_'
                and p.prosrc ~* 'create\s+(temp|temporary)\s+table'
        loop
            continue when exists (select from unnest(created) c where to_regclass(c) is not null);
            begin
                for pragma in select m from @extschema@.plpgsql_make_pragma(f, fatal_errors => false) m loop
                    name := substring(pragma from '^table:\s*([^(]+?)\s*\(');
                    if name is not null and to_regclass(name) is null then
                        ddl := format('create temp table %s %s', name, substring(pragma from '^table:\s*[^(]+(\(.*\))\s*$'));
                        execute ddl;
                        perform set_config('sqiff.temp_tables', coalesce(current_setting('sqiff.temp_tables', true), '') || ddl || E';\n', false);
                        added := added + 1;
                    end if;
                end loop;
            exception when others then
                null;
            end;
        end loop;
        exit when added = 0 or rounds >= 10;
    end loop;
end
$$
