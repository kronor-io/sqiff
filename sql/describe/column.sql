select
    'pg_class'::regclass::oid as classid,
    a.attrelid as objid,
    a.attnum::int as objsubid,
    a.attrelid as relid,
    jsonb_build_object(
        'position', a.attnum,
        'type', format_type(a.atttypid, a.atttypmod),
        -- The schema is left out for pg_catalog, where the built-in collations are.
        'collation', case when a.attcollation <> t.typcollation then jsonb_build_object(
            'schema', nullif(con.nspname, 'pg_catalog'),
            'name', co.collname
        ) end,
        'not_null', case
            when nn.oid is not null then jsonb_build_object(
                'validated', nn.convalidated,
                -- The name, unless it is the one PostgreSQL gives a not-null constraint.
                'name', case
                    when nn.conname ~ '_not_null[0-9]*$'
                        and (
                            starts_with(nn.conname, c.relname || '_')
                            -- PostgreSQL shortens the table and column names to fit a default name in 63 bytes.
                            or (
                                octet_length(nn.conname) = 63
                                and exists (
                                    select
                                    from generate_series(1, length(c.relname)) k
                                    cross join lateral (select regexp_replace(nn.conname, '_not_null[0-9]*$', '') as stem) s
                                    where length(s.stem) - k - 1 between 1 and length(a.attname)
                                        and left(c.relname, k) || '_' || left(a.attname, length(s.stem) - k - 1) = s.stem
                                )
                            )
                        )
                        then null
                    else nn.conname
                end
            )
        end,
        'identity', case a.attidentity
            when 'a' then 'always'
            when 'd' then 'by default'
        end,
        -- The identity sequence's options that differ from the defaults.
        'identity_options', case when a.attidentity <> '' then (
            select to_jsonb(array_remove(array[
                case when s.seqincrement <> 1 then 'increment by ' || s.seqincrement end,
                case when s.seqmin <> 1 then 'minvalue ' || s.seqmin end,
                case
                    when s.seqmax <> case s.seqtypid when 'int2'::regtype then 32767 when 'int4'::regtype then 2147483647 else 9223372036854775807 end
                        then 'maxvalue ' || s.seqmax
                end,
                case when s.seqstart <> s.seqmin then 'start with ' || s.seqstart end,
                case when s.seqcache <> 1 then 'cache ' || s.seqcache end,
                case when s.seqcycle then 'cycle' end
            ], null))
            from pg_depend d
            join pg_sequence s on s.seqrelid = d.objid
            where d.classid = 'pg_class'::regclass
                and d.refclassid = 'pg_class'::regclass
                and d.refobjid = a.attrelid
                and d.refobjsubid = a.attnum
                and d.deptype = 'i'
        ) end,
        'generated', case a.attgenerated
            when 's' then 'stored'
            when 'v' then 'virtual'
        end,
        -- The generation expression, or the default.
        'expression', pg_get_expr(ad.adbin, ad.adrelid),
        'rewrites_on_add', exists (
                select
                from regexp_matches(pg_get_expr(ad.adbin, ad.adrelid), '([[:alpha:]_][[:alnum:]_$]*)\(', 'g') m
                join pg_proc p on p.proname = m[1]
                where a.attgenerated = '' and p.provolatile = 'v'
            )
            or (t.typtype = 'd' and exists (select from pg_constraint dc where dc.contypid = a.atttypid))
    )::text as definition
from pg_attribute a
join pg_class c on c.oid = a.attrelid
join pg_namespace n on n.oid = c.relnamespace
join pg_type t on t.oid = a.atttypid
left join pg_collation co on co.oid = a.attcollation
left join pg_namespace con on con.oid = co.collnamespace
left join pg_attrdef ad on ad.adrelid = a.attrelid and ad.adnum = a.attnum
left join pg_constraint nn on nn.conrelid = a.attrelid and nn.contype = 'n' and nn.conkey = array[a.attnum]
where a.attnum > 0
    and not a.attisdropped
    and c.relkind in ('r', 'p')
    and ($2 or not c.relispartition)
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
