select
    'pg_constraint'::regclass::oid as classid,
    con.oid as objid,
    0 as objsubid,
    pg_get_constraintdef(con.oid) as definition
from pg_constraint con
join pg_namespace n on n.oid = con.connamespace
where con.contype <> 'n'
    and con.conparentid = 0
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
