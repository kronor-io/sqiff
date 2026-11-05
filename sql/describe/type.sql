select
    'pg_type'::regclass::oid as classid,
    t.oid as objid,
    0 as objsubid,
    case t.typtype
        when 'e' then
            'enum ('
            || (
                select string_agg(quote_literal(e.enumlabel), ', ' order by e.enumsortorder)
                from pg_enum e
                where e.enumtypid = t.oid
            )
            || ')'
        when 'd' then
            concat_ws(
                ' ',
                'domain ' || format_type(t.typbasetype, t.typtypmod),
                case when t.typnotnull then 'not null' end,
                'default ' || t.typdefault
            )
        when 'c' then
            'composite ('
            || (
                select string_agg(quote_ident(a.attname) || ' ' || format_type(a.atttypid, a.atttypmod), ', ' order by a.attnum)
                from pg_attribute a
                where a.attrelid = t.typrelid
                    and a.attnum > 0
                    and not a.attisdropped
            )
            || ')'
        when 'r' then
            'range (subtype = ' || (select format_type(r.rngsubtype, null) from pg_range r where r.rngtypid = t.oid) || ')'
    end as definition
from pg_type t
join pg_namespace n on n.oid = t.typnamespace
where t.typtype in ('e', 'd', 'c', 'r')
    and (t.typtype <> 'c' or exists (select from pg_class k where k.oid = t.typrelid and k.relkind = 'c'))
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
