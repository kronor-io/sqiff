select
    'pg_constraint'::regclass::oid as classid,
    con.oid as objid,
    0 as objsubid,
    nullif(con.conrelid, 0) as relid,
    jsonb_build_object(
        'clause', pg_get_constraintdef(con.oid),
        'kind', con.contype,
        'validated', con.convalidated,
        'columns', (
            select jsonb_agg(a.attname order by k.n)
            from unnest(con.conkey) with ordinality k (attnum, n)
            join pg_attribute a on a.attrelid = con.conrelid and a.attnum = k.attnum
        ),
        'references', case
            when con.contype = 'f' then (
                select jsonb_build_object(
                    'schema', rn.nspname,
                    'table', rc.relname,
                    'columns', (
                        select jsonb_agg(a.attname order by k.n)
                        from unnest(con.confkey) with ordinality k (attnum, n)
                        join pg_attribute a on a.attrelid = con.confrelid and a.attnum = k.attnum
                    )
                )
                from pg_class rc
                join pg_namespace rn on rn.oid = rc.relnamespace
                where rc.oid = con.confrelid
            )
        end
    )::text as definition
from pg_constraint con
join pg_namespace n on n.oid = con.connamespace
left join pg_class tc on tc.oid = con.conrelid
where con.contype not in ('n', 't')
    and ($2 or not coalesce(tc.relispartition, false))
    and con.conparentid = 0
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
    -- For one table ($3), only its constraints, found by index.
    and ($3::text is null or con.conrelid = to_regclass($3::text))
