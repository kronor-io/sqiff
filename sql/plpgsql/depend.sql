-- What the bodies of the functions in $1 use, as plpgsql_check's static analysis sees it,
-- as (function, class id, object id). $2 holds each function's trigger table, null for
-- other functions. @extschema@ is replaced with plpgsql_check's schema.
with f as (
    select u.oid::oid as oid, u.relid::oid as relid
    from unnest($1::int8[], $2::int8[]) u(oid, relid)
)
select distinct f.oid::int8, k.classid::int8, d.oid::int8
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
where k.classid is not null
    and d.oid >= 16384
    and (k.classid, d.oid) <> ('pg_proc'::regclass::oid, f.oid)
