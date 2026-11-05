select
    'pg_namespace'::regclass::oid as classid,
    n.oid as objid,
    0 as objsubid,
    '' as definition
from pg_namespace n
where n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
