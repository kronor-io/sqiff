-- PL/pgSQL functions and procedures the describe queries cover. A trigger function gets
-- a table one of its triggers is on; plpgsql_check needs one to analyse its body. The
-- last column estimates how long the analysis takes: plpgsql_check plans each query, and
-- planning a query on a partitioned table opens every partition, so it counts the
-- partitions of the partitioned tables the body names.
with partitions as materialized (
    select c.relname::text as relname, count(*) as n
    from pg_class c
    join pg_inherits h on h.inhparent = c.oid
    where c.relkind = 'p'
    group by c.relname
    having count(*) >= 100
)
select
    p.oid::int8,
    p.oid::regprocedure::text,
    p.prorettype = 'trigger'::regtype,
    t.tgrelid::int8,
    e.cost::int8
from pg_proc p
join pg_language l on l.oid = p.prolang
join pg_namespace n on n.oid = p.pronamespace
left join (
    select distinct on (g.tgfoid) g.tgfoid, g.tgrelid
    from pg_trigger g
    order by g.tgfoid, g.oid
) t on t.tgfoid = p.oid
cross join lateral (
    select coalesce(sum(partitions.n), 0) as cost
    from partitions, (select p.prosrc || '' as src) b
    where strpos(b.src, partitions.relname) > 0
) e
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
