select
    'pg_policy'::regclass::oid as classid,
    p.oid as objid,
    0 as objsubid,
    p.polrelid as relid,
    concat_ws(
        E'\n',
        case when p.polpermissive then 'as permissive' else 'as restrictive' end,
        'for ' || case p.polcmd
            when 'r' then 'select'
            when 'a' then 'insert'
            when 'w' then 'update'
            when 'd' then 'delete'
            else 'all'
        end,
        'to ' || (
            select string_agg(r.name, ', ' order by r.name)
            from (
                select case when role = 0 then 'public' else quote_ident(pg_get_userbyid(role)) end as name
                from unnest(p.polroles) role
            ) r
        ),
        'using (' || pg_get_expr(p.polqual, p.polrelid) || ')',
        'with check (' || pg_get_expr(p.polwithcheck, p.polrelid) || ')'
    ) as definition
from pg_policy p
join pg_class c on c.oid = p.polrelid
join pg_namespace n on n.oid = c.relnamespace
where n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
