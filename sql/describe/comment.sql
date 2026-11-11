select
    d.classoid as classid,
    d.objoid as objid,
    d.objsubid,
    coalesce(i.indrelid, c.oid, nullif(k.conrelid, 0), t.tgrelid, p.polrelid) as relid,
    d.description as definition
from pg_description d
left join pg_class c on d.classoid = 'pg_class'::regclass and c.oid = d.objoid
left join pg_index i on i.indexrelid = c.oid
left join pg_constraint k on d.classoid = 'pg_constraint'::regclass and k.oid = d.objoid
left join pg_trigger t on d.classoid = 'pg_trigger'::regclass and t.oid = d.objoid
left join pg_policy p on d.classoid = 'pg_policy'::regclass and p.oid = d.objoid
left join pg_proc f on d.classoid = 'pg_proc'::regclass and f.oid = d.objoid
left join pg_type y on d.classoid = 'pg_type'::regclass and y.oid = d.objoid
left join pg_namespace s on d.classoid = 'pg_namespace'::regclass and s.oid = d.objoid
left join pg_class r on r.oid = coalesce(i.indrelid, c.oid, nullif(k.conrelid, 0), t.tgrelid, p.polrelid)
join pg_namespace n on n.oid = coalesce(r.relnamespace, f.pronamespace, y.typnamespace, k.connamespace, s.oid)
where d.classoid in (
        'pg_class'::regclass,
        'pg_constraint'::regclass,
        'pg_trigger'::regclass,
        'pg_policy'::regclass,
        'pg_proc'::regclass,
        'pg_type'::regclass,
        'pg_namespace'::regclass
    )
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
    and not (y.oid is not null and y.typrelid <> 0 and y.typtype = 'c' and exists (select from pg_class x where x.oid = y.typrelid and x.relkind <> 'c'))
