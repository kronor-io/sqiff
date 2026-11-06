select
    'pg_constraint'::regclass::oid as classid,
    con.oid as objid,
    0 as objsubid,
    nullif(con.conrelid, 0) as relid,
    pg_get_constraintdef(con.oid) as definition
from pg_constraint con
join pg_namespace n on n.oid = con.connamespace
left join pg_class tc on tc.oid = con.conrelid
where con.contype not in ('n', 't')
    and ($2 or not coalesce(tc.relispartition, false))
    and con.conparentid = 0
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
