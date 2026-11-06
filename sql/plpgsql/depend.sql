-- What the bodies of the functions in $1 use, as plpgsql_check's static analysis sees it.
-- $2 holds each function's trigger table, null for other functions. @extschema@ is
-- replaced with plpgsql_check's schema.
with f as (
    select u.oid::oid as oid, u.relid::oid as relid
    from unnest($1::int8[], $2::int8[]) u(oid, relid)
)
select distinct
    a.type, a.object_names, a.object_args,
    b.type, b.object_names, b.object_args
from f
cross join lateral (
    select d.type, d.oid
    from @extschema@.plpgsql_show_dependency_tb(f.oid::regprocedure) d
    where f.relid is null
    union all
    select d.type, d.oid
    from @extschema@.plpgsql_show_dependency_tb(f.oid::regprocedure, f.relid::regclass) d
    where f.relid is not null
) d
cross join lateral (
    select case d.type
        when 'RELATION' then 'pg_class'::regclass::oid
        when 'FUNCTION' then 'pg_proc'::regclass::oid
        when 'OPERATOR' then 'pg_operator'::regclass::oid
        when 'TYPE' then 'pg_type'::regclass::oid
    end as classid
) k
cross join lateral pg_identify_object_as_address('pg_proc'::regclass, f.oid, 0) a
cross join lateral pg_identify_object_as_address(k.classid, d.oid, 0) b
cross join lateral pg_identify_object(k.classid, d.oid, 0) ib
where k.classid is not null
    and d.oid >= 16384
    and (k.classid, d.oid) <> ('pg_proc'::regclass::oid, f.oid)
    and not coalesce(ib.schema = any($3), false)
