select
    'pg_class'::regclass::oid as classid,
    a.attrelid as objid,
    a.attnum::int as objsubid,
    concat_ws(
        E'\n',
        'type ' || format_type(a.atttypid, a.atttypmod),
        case when a.attcollation <> t.typcollation then 'collate ' || quote_ident(co.collname) end,
        case when a.attnotnull then 'not null' end,
        case a.attidentity
            when 'a' then 'identity always'
            when 'd' then 'identity by default'
        end,
        case a.attgenerated
            when 's' then 'generated stored ' || pg_get_expr(ad.adbin, ad.adrelid)
            when 'v' then 'generated virtual ' || pg_get_expr(ad.adbin, ad.adrelid)
            else 'default ' || pg_get_expr(ad.adbin, ad.adrelid)
        end
    ) as definition
from pg_attribute a
join pg_class c on c.oid = a.attrelid
join pg_namespace n on n.oid = c.relnamespace
join pg_type t on t.oid = a.atttypid
left join pg_collation co on co.oid = a.attcollation
left join pg_attrdef ad on ad.adrelid = a.attrelid and ad.adnum = a.attnum
where a.attnum > 0
    and not a.attisdropped
    and c.relkind in ('r', 'p')
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
