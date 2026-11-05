select
    'pg_trigger'::regclass::oid as classid,
    t.oid as objid,
    0 as objsubid,
    pg_get_triggerdef(t.oid) as definition
from pg_trigger t
join pg_class c on c.oid = t.tgrelid
join pg_namespace n on n.oid = c.relnamespace
where not t.tgisinternal
    and t.tgparentid = 0
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
