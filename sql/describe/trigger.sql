select
    'pg_trigger'::regclass::oid as classid,
    t.oid as objid,
    0 as objsubid,
    t.tgrelid as relid,
    pg_get_triggerdef(t.oid)
        || case t.tgenabled
            when 'D' then E';\nALTER TABLE ' || t.tgrelid::regclass::text || ' DISABLE TRIGGER ' || quote_ident(t.tgname)
            when 'R' then E';\nALTER TABLE ' || t.tgrelid::regclass::text || ' ENABLE REPLICA TRIGGER ' || quote_ident(t.tgname)
            when 'A' then E';\nALTER TABLE ' || t.tgrelid::regclass::text || ' ENABLE ALWAYS TRIGGER ' || quote_ident(t.tgname)
            else ''
        end as definition
from pg_trigger t
join pg_class c on c.oid = t.tgrelid
join pg_namespace n on n.oid = c.relnamespace
where not t.tgisinternal
    and ($2 or not c.relispartition)
    and t.tgparentid = 0
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
