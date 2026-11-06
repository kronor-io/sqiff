select
    'pg_proc'::regclass::oid as classid,
    p.oid as objid,
    0 as objsubid,
    null::oid as relid,
    case when p.prokind = 'a' then 'aggregate' else pg_get_functiondef(p.oid) end as definition
from pg_proc p
join pg_namespace n on n.oid = p.pronamespace
where n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
