-- View rules, column defaults, table row types and arrays of either stand for the
-- view, column, table or element type they belong to.
with raw as (
    select d.classid, d.objid, d.objsubid, d.refclassid, d.refobjid, d.refobjsubid
    from pg_depend d
    where d.deptype in ('n', 'a', 'i')
        and d.objid >= 16384
        and d.refobjid >= 16384
),
edge as (
    select distinct
        a.classid, a.objid, a.objsubid,
        b.classid as refclassid, b.objid as refobjid, b.objsubid as refobjsubid
    from raw
    cross join lateral (
        select
            case
                when rw.ev_class is not null or ad.adrelid is not null or tr.typrelid is not null or te.typrelid is not null
                    then 'pg_class'::regclass::oid
                when ta.typelem is not null then 'pg_type'::regclass::oid
                else raw.classid
            end as classid,
            coalesce(rw.ev_class, ad.adrelid, tr.typrelid, te.typrelid, ta.typelem, raw.objid) as objid,
            case
                when ad.adrelid is not null then ad.adnum::int
                when rw.ev_class is not null or tr.typrelid is not null or ta.typelem is not null then 0
                else raw.objsubid
            end as objsubid
        from (select) one
        left join pg_rewrite rw on raw.classid = 'pg_rewrite'::regclass and rw.oid = raw.objid
        left join pg_attrdef ad on raw.classid = 'pg_attrdef'::regclass and ad.oid = raw.objid
        left join pg_type tr
            on raw.classid = 'pg_type'::regclass
            and tr.oid = raw.objid
            and exists (select from pg_class k where k.oid = tr.typrelid and k.relkind <> 'c')
        left join pg_type ta
            on raw.classid = 'pg_type'::regclass
            and ta.oid = raw.objid
            and ta.typcategory = 'A'
            and ta.typelem <> 0
        left join pg_type te
            on te.oid = ta.typelem
            and exists (select from pg_class k where k.oid = te.typrelid and k.relkind <> 'c')
    ) a
    cross join lateral (
        select
            case
                when rw.ev_class is not null or ad.adrelid is not null or tr.typrelid is not null or te.typrelid is not null
                    then 'pg_class'::regclass::oid
                when ta.typelem is not null then 'pg_type'::regclass::oid
                else raw.refclassid
            end as classid,
            coalesce(rw.ev_class, ad.adrelid, tr.typrelid, te.typrelid, ta.typelem, raw.refobjid) as objid,
            case
                when ad.adrelid is not null then ad.adnum::int
                when rw.ev_class is not null or tr.typrelid is not null or ta.typelem is not null then 0
                else raw.refobjsubid
            end as objsubid
        from (select) one
        left join pg_rewrite rw on raw.refclassid = 'pg_rewrite'::regclass and rw.oid = raw.refobjid
        left join pg_attrdef ad on raw.refclassid = 'pg_attrdef'::regclass and ad.oid = raw.refobjid
        left join pg_type tr
            on raw.refclassid = 'pg_type'::regclass
            and tr.oid = raw.refobjid
            and exists (select from pg_class k where k.oid = tr.typrelid and k.relkind <> 'c')
        left join pg_type ta
            on raw.refclassid = 'pg_type'::regclass
            and ta.oid = raw.refobjid
            and ta.typcategory = 'A'
            and ta.typelem <> 0
        left join pg_type te
            on te.oid = ta.typelem
            and exists (select from pg_class k where k.oid = te.typrelid and k.relkind <> 'c')
    ) b
    where (a.classid, a.objid, a.objsubid) is distinct from (b.classid, b.objid, b.objsubid)
)
select
    a.type, a.object_names, a.object_args,
    b.type, b.object_names, b.object_args
from edge
cross join lateral pg_identify_object_as_address(edge.classid, edge.objid, edge.objsubid) a
cross join lateral pg_identify_object_as_address(edge.refclassid, edge.refobjid, edge.refobjsubid) b
cross join lateral pg_identify_object(edge.classid, edge.objid, edge.objsubid) ia
cross join lateral pg_identify_object(edge.refclassid, edge.refobjid, edge.refobjsubid) ib
where not coalesce(ia.schema = any($1), false)
    and not coalesce(ib.schema = any($1), false)
