select
    'pg_class'::regclass::oid as classid,
    a.attrelid as objid,
    a.attnum::int as objsubid,
    a.attrelid as relid,
    concat_ws(
        E'\n',
        'type ' || format_type(a.atttypid, a.atttypmod),
        case when a.attcollation <> t.typcollation then 'collate ' || quote_ident(co.collname) end,
        case
            when nn.oid is not null then
                'not null'
                || case when nn.convalidated then '' else ' not valid' end
                || case
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
                        then ''
                    else ' constraint ' || nn.conname
                end
        end,
        case a.attidentity
            when 'a' then 'identity always'
            when 'd' then 'identity by default'
        end,
        case when a.attidentity <> '' then (
            select 'identity options ' || nullif(concat_ws(
                ', ',
                case when s.seqincrement <> 1 then 'increment by ' || s.seqincrement end,
                case when s.seqmin <> 1 then 'minvalue ' || s.seqmin end,
                case
                    when s.seqmax <> case s.seqtypid when 'int2'::regtype then 32767 when 'int4'::regtype then 2147483647 else 9223372036854775807 end
                        then 'maxvalue ' || s.seqmax
                end,
                case when s.seqstart <> s.seqmin then 'start with ' || s.seqstart end,
                case when s.seqcache <> 1 then 'cache ' || s.seqcache end,
                case when s.seqcycle then 'cycle' end
            ), '')
            from pg_depend d
            join pg_sequence s on s.seqrelid = d.objid
            where d.classid = 'pg_class'::regclass
                and d.refclassid = 'pg_class'::regclass
                and d.refobjid = a.attrelid
                and d.refobjsubid = a.attnum
                and d.deptype = 'i'
        ) end,
        case a.attgenerated
            when 's' then 'generated stored ' || pg_get_expr(ad.adbin, ad.adrelid)
            when 'v' then 'generated virtual ' || pg_get_expr(ad.adbin, ad.adrelid)
            else 'default ' || pg_get_expr(ad.adbin, ad.adrelid)
        end,
        case
            when exists (
                select
                from regexp_matches(pg_get_expr(ad.adbin, ad.adrelid), '([[:alpha:]_][[:alnum:]_$]*)\(', 'g') m
                join pg_proc p on p.proname = m[1]
                where a.attgenerated = '' and p.provolatile = 'v'
            )
                or (t.typtype = 'd' and exists (select from pg_constraint dc where dc.contypid = a.atttypid))
                then 'rewrites the table when added'
        end
    ) as definition
from pg_attribute a
join pg_class c on c.oid = a.attrelid
join pg_namespace n on n.oid = c.relnamespace
join pg_type t on t.oid = a.atttypid
left join pg_collation co on co.oid = a.attcollation
left join pg_attrdef ad on ad.adrelid = a.attrelid and ad.adnum = a.attnum
left join pg_constraint nn on nn.conrelid = a.attrelid and nn.contype = 'n' and nn.conkey = array[a.attnum]
where a.attnum > 0
    and not a.attisdropped
    and c.relkind in ('r', 'p')
    and ($2 or not c.relispartition)
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
