with acl as (
    select 'pg_class'::regclass::oid as classid, c.oid as objid, c.relacl as acl
    from pg_class c
    join pg_namespace n on n.oid = c.relnamespace
    where c.relkind in ('r', 'p', 'v', 'm', 'S')
        and n.nspname <> 'information_schema'
        and n.nspname !~ '^pg_'
    union all
    select 'pg_proc'::regclass::oid, p.oid, p.proacl
    from pg_proc p
    join pg_namespace n on n.oid = p.pronamespace
    where n.nspname <> 'information_schema'
        and n.nspname !~ '^pg_'
    union all
    select 'pg_namespace'::regclass::oid, n.oid, n.nspacl
    from pg_namespace n
    where n.nspname <> 'information_schema'
        and n.nspname !~ '^pg_'
)
select
    acl.classid,
    acl.objid,
    0 as objsubid,
    coalesce(
        (
            select string_agg(g.line, E'\n' order by g.line)
            from (
                select
                    case when x.grantee = 0 then 'public' else pg_get_userbyid(x.grantee) end
                    || ' '
                    || x.privilege_type
                    || case when x.is_grantable then ' with grant option' else '' end as line
                from aclexplode(acl.acl) x
            ) g
        ),
        'default'
    ) as definition
from acl
