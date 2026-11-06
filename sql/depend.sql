-- Edges between the objects in $1, $2 and $3 (class, object and sub-object ids), as one
-- row of arrays. View rules, column defaults, table row types and arrays of either stand
-- for the view, column, table or element type they belong to.
with remap (classid, objid, toclassid, toobjid, tosubid) as (
    select 'pg_rewrite'::regclass::oid, rw.oid, 'pg_class'::regclass::oid, rw.ev_class, 0
    from pg_rewrite rw
    union all
    select 'pg_attrdef'::regclass::oid, ad.oid, 'pg_class'::regclass::oid, ad.adrelid, ad.adnum::int
    from pg_attrdef ad
    union all
    select 'pg_type'::regclass::oid, t.oid, 'pg_class'::regclass::oid, k.oid, 0
    from pg_type t
    join pg_class k on k.oid = t.typrelid and k.relkind <> 'c'
    union all
    select
        'pg_type'::regclass::oid,
        ta.oid,
        case when k.oid is not null then 'pg_class'::regclass::oid else 'pg_type'::regclass::oid end,
        coalesce(k.oid, ta.typelem),
        0
    from pg_type ta
    left join pg_type te on te.oid = ta.typelem
    left join pg_class k on k.oid = te.typrelid and k.relkind <> 'c'
    where ta.typcategory = 'A'
        and ta.typelem <> 0
),
edge as (
    select distinct
        coalesce(ma.toclassid, d.classid) as classid,
        coalesce(ma.toobjid, d.objid) as objid,
        coalesce(ma.tosubid, d.objsubid) as objsubid,
        coalesce(mb.toclassid, d.refclassid) as refclassid,
        coalesce(mb.toobjid, d.refobjid) as refobjid,
        coalesce(mb.tosubid, d.refobjsubid) as refobjsubid
    from pg_depend d
    left join remap ma on ma.classid = d.classid and ma.objid = d.objid
    left join remap mb on mb.classid = d.refclassid and mb.objid = d.refobjid
    where d.deptype in ('n', 'a', 'i')
        and d.objid >= 16384
        and d.refobjid >= 16384
),
described as (
    select k.classid::oid, k.objid::oid, k.objsubid
    from unnest($1::int8[], $2::int8[], $3::int4[]) as k (classid, objid, objsubid)
)
select
    coalesce(array_agg(e.classid::int8), '{}'),
    coalesce(array_agg(e.objid::int8), '{}'),
    coalesce(array_agg(e.objsubid::int4), '{}'),
    coalesce(array_agg(e.refclassid::int8), '{}'),
    coalesce(array_agg(e.refobjid::int8), '{}'),
    coalesce(array_agg(e.refobjsubid::int4), '{}')
from edge e
where (e.classid, e.objid, e.objsubid) is distinct from (e.refclassid, e.refobjid, e.refobjsubid)
    and exists (select from described k where k.classid = e.classid and k.objid = e.objid and k.objsubid = e.objsubid)
    and exists (select from described k where k.classid = e.refclassid and k.objid = e.refobjid and k.objsubid = e.refobjsubid)
