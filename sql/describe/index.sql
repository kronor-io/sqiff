select
    'pg_class'::regclass::oid as classid,
    i.indexrelid as objid,
    0 as objsubid,
    pg_get_indexdef(i.indexrelid) as definition
from pg_index i
join pg_class c on c.oid = i.indexrelid
join pg_namespace n on n.oid = c.relnamespace
where n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
    and not exists (
        select
        from pg_depend d
        where d.classid = 'pg_class'::regclass
            and d.objid = i.indexrelid
            and d.refclassid = 'pg_constraint'::regclass
            and d.deptype = 'i'
    )
    and not exists (select from pg_inherits h where h.inhrelid = i.indexrelid)
