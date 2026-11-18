with acl as (
    select 'pg_class'::regclass::oid as classid, c.oid as objid, c.relacl as acl, c.relowner as owner
    from pg_class c
    join pg_namespace n on n.oid = c.relnamespace
    where c.relkind in ('r', 'p', 'v', 'm', 'S')
        and ($2 or not c.relispartition)
        and n.nspname <> 'information_schema'
        and n.nspname !~ '^pg_'
    union all
    select 'pg_proc'::regclass::oid, p.oid, p.proacl, p.proowner
    from pg_proc p
    join pg_namespace n on n.oid = p.pronamespace
    where n.nspname <> 'information_schema'
        and n.nspname !~ '^pg_'
    union all
    select 'pg_namespace'::regclass::oid, n.oid, n.nspacl, n.nspowner
    from pg_namespace n
    where n.nspname <> 'information_schema'
        and n.nspname !~ '^pg_'
)
select
    acl.classid,
    acl.objid,
    0 as objsubid,
    case when acl.classid = 'pg_class'::regclass then acl.objid end as relid,
    -- JSON null for the defaults, else every grant, the owner's included.
    case
        when acl.acl is null then 'null'
        else coalesce(
            (
                select jsonb_agg(
                    jsonb_build_object(
                        'grantee', case when x.grantee = 0 then null else pg_get_userbyid(x.grantee) end,
                        'privilege', x.privilege_type,
                        'grantable', x.is_grantable,
                        -- Null when the owner granted it, so owners need not match.
                        'grantor', case when x.grantor = acl.owner then null else pg_get_userbyid(x.grantor) end
                    )
                )
                from aclexplode(acl.acl) x
            ),
            '[]'::jsonb
        )::text
    end as definition
from acl
