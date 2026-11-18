select
    'pg_type'::regclass::oid as classid,
    t.oid as objid,
    0 as objsubid,
    null::oid as relid,
    case t.typtype
        when 'e' then jsonb_build_object(
            'kind', 'enum',
            'labels', (
                select jsonb_agg(e.enumlabel order by e.enumsortorder)
                from pg_enum e
                where e.enumtypid = t.oid
            )
        )
        when 'd' then jsonb_build_object(
            'kind', 'domain',
            'base', format_type(t.typbasetype, t.typtypmod),
            'not_null', t.typnotnull,
            'default', t.typdefault
        )
        when 'c' then jsonb_build_object(
            'kind', 'composite',
            'attributes', (
                select jsonb_agg(jsonb_build_array(a.attname, format_type(a.atttypid, a.atttypmod)) order by a.attnum)
                from pg_attribute a
                where a.attrelid = t.typrelid
                    and a.attnum > 0
                    and not a.attisdropped
            )
        )
        when 'r' then jsonb_build_object(
            'kind', 'range',
            'subtype', (select format_type(r.rngsubtype, null) from pg_range r where r.rngtypid = t.oid)
        )
    end::text as definition
from pg_type t
join pg_namespace n on n.oid = t.typnamespace
where t.typtype in ('e', 'd', 'c', 'r')
    and (t.typtype <> 'c' or exists (select from pg_class k where k.oid = t.typrelid and k.relkind = 'c'))
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
