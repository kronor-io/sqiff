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
            'subtype', (select format_type(r.rngsubtype, null) from pg_range r where r.rngtypid = t.oid),
            -- Every option create type ... as range takes, in its order, the defaults left out.
            'options', (
                select (select jsonb_agg(o) from jsonb_array_elements(jsonb_build_array(
                    jsonb_build_array('subtype', format_type(r.rngsubtype, null)),
                    case when not opc.opcdefault then jsonb_build_array('subtype_opclass', quote_ident(opn.nspname) || '.' || quote_ident(opc.opcname)) end,
                    case when r.rngcollation <> 0 and r.rngcollation <> st.typcollation then jsonb_build_array('collation', quote_ident(cn.nspname) || '.' || quote_ident(co.collname)) end,
                    case when r.rngcanonical <> 0 then jsonb_build_array('canonical', r.rngcanonical::regproc::text) end,
                    case when r.rngsubdiff <> 0 then jsonb_build_array('subtype_diff', r.rngsubdiff::regproc::text) end,
                    jsonb_build_array('multirange_type_name', format_type(r.rngmultitypid, null))
                )) o where o <> 'null'::jsonb)
                from pg_range r
                join pg_type st on st.oid = r.rngsubtype
                join pg_opclass opc on opc.oid = r.rngsubopc
                join pg_namespace opn on opn.oid = opc.opcnamespace
                left join pg_collation co on co.oid = r.rngcollation
                left join pg_namespace cn on cn.oid = co.collnamespace
                where r.rngtypid = t.oid
            )
        )
    end::text as definition
from pg_type t
join pg_namespace n on n.oid = t.typnamespace
where t.typtype in ('e', 'd', 'c', 'r')
    and (t.typtype <> 'c' or exists (select from pg_class k where k.oid = t.typrelid and k.relkind = 'c'))
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
