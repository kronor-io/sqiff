-- PL/pgSQL functions and procedures the describe queries cover. A trigger function gets
-- a table one of its triggers is on; plpgsql_check needs one to analyse its body.
select
    p.oid::int8,
    p.oid::regprocedure::text,
    p.prorettype = 'trigger'::regtype,
    t.tgrelid::int8
from pg_proc p
join pg_language l on l.oid = p.prolang
join pg_namespace n on n.oid = p.pronamespace
left join (
    select distinct on (g.tgfoid) g.tgfoid, g.tgrelid
    from pg_trigger g
    order by g.tgfoid, g.oid
) t on t.tgfoid = p.oid
where l.lanname = 'plpgsql'
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
    and not n.nspname = any($1)
    and not exists (
        select
        from pg_depend e
        where e.deptype = 'e'
            and e.classid = 'pg_proc'::regclass
            and e.objid = p.oid
    )
order by p.oid
