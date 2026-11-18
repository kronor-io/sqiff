select
    'pg_class'::regclass::oid as classid,
    s.seqrelid as objid,
    0 as objsubid,
    s.seqrelid as relid,
    jsonb_build_object(
        'type', format_type(s.seqtypid, null),
        'increment', s.seqincrement,
        'min', s.seqmin,
        'max', s.seqmax,
        'start', s.seqstart,
        'cache', s.seqcache,
        'cycle', s.seqcycle,
        -- The column whose drop takes the sequence with it.
        'owned_by', (
            select jsonb_build_object('schema', rn.nspname, 'table', rc.relname, 'column', ra.attname)
            from pg_depend d
            join pg_class rc on rc.oid = d.refobjid
            join pg_namespace rn on rn.oid = rc.relnamespace
            join pg_attribute ra on ra.attrelid = d.refobjid and ra.attnum = d.refobjsubid
            where d.classid = 'pg_class'::regclass
                and d.objid = s.seqrelid
                and d.refclassid = 'pg_class'::regclass
                and d.deptype = 'a'
        )
    )::text as definition
from pg_sequence s
join pg_class c on c.oid = s.seqrelid
join pg_namespace n on n.oid = c.relnamespace
where n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
    and not exists (
        select
        from pg_depend d
        where d.classid = 'pg_class'::regclass
            and d.objid = s.seqrelid
            and d.deptype = 'i'
    )
