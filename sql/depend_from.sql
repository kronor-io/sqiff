-- Like depend.sql, limited to the edges reachable from $1, $2 and $3: those whose
-- referenced end is one of them or, repeatedly, the dependent end of such an edge.
-- Each step finds the pg_depend rows that reference an object through the reference
-- index, directly or through the view rules, column defaults and types that stand for
-- it. From a relation it also steps to its columns, which views and constraints depend
-- on instead of the relation; those steps are not edges. It also follows objects that
-- are not described; the caller drops their edges.
with recursive
-- The starts, then every step, as (dependent, referenced, whether it is an edge); a start
-- has no referenced end.
step (classid, objid, objsubid, refclassid, refobjid, refobjsubid, edge) as (
    select s.classid::oid, s.objid::oid, s.objsubid, null::oid, null::oid, null::int4, false
    from unnest($1::int8[], $2::int8[], $3::int4[]) as s (classid, objid, objsubid)
    union
    select x.classid, x.objid, x.objsubid, p.classid, p.objid, p.objsubid, x.edge
    from step p
    cross join lateral (
        select 'pg_class'::regclass::oid as classid, a.attrelid as objid, a.attnum::int4 as objsubid, false as edge
        from pg_attribute a
        where p.classid = 'pg_class'::regclass and p.objsubid = 0 and a.attrelid = p.objid and a.attnum > 0 and not a.attisdropped
        union all
        select e.classid, e.objid, e.objsubid, true
        from (
            -- What stands for p: p itself, its view rules, its column's default, its row type
            -- and that type's array, or its array type.
            select p.classid as classid, p.objid as objid
            union all
            select 'pg_rewrite'::regclass::oid, rw.oid
            from pg_rewrite rw
            where p.classid = 'pg_class'::regclass and p.objsubid = 0 and rw.ev_class = p.objid
            union all
            select 'pg_attrdef'::regclass::oid, ad.oid
            from pg_attrdef ad
            where p.classid = 'pg_class'::regclass and p.objsubid > 0 and ad.adrelid = p.objid and ad.adnum = p.objsubid
            union all
            select 'pg_type'::regclass::oid, t.oid
            from pg_class k
            join pg_type rt on rt.oid = k.reltype
            cross join lateral (values (rt.oid), (rt.typarray)) t (oid)
            where p.classid = 'pg_class'::regclass and p.objsubid = 0 and k.oid = p.objid and k.relkind <> 'c' and t.oid <> 0
            union all
            select 'pg_type'::regclass::oid, te.typarray
            from pg_type te
            where p.classid = 'pg_type'::regclass and te.oid = p.objid and te.typarray <> 0
        ) r
        join pg_depend d on d.refclassid = r.classid and d.refobjid = r.objid
        -- The dependent and referenced ends after the same remap as depend.sql.
        cross join lateral (
            select coalesce(ma.toclassid, d.classid) as classid, coalesce(ma.toobjid, d.objid) as objid, coalesce(ma.tosubid, d.objsubid) as objsubid
            from (select) one
            left join lateral (
                select 'pg_class'::regclass::oid as toclassid, rw.ev_class as toobjid, 0 as tosubid
                from pg_rewrite rw
                where d.classid = 'pg_rewrite'::regclass and rw.oid = d.objid
                union all
                select 'pg_class'::regclass::oid, ad.adrelid, ad.adnum::int
                from pg_attrdef ad
                where d.classid = 'pg_attrdef'::regclass and ad.oid = d.objid
                union all
                select 'pg_class'::regclass::oid, k.oid, 0
                from pg_type t
                join pg_class k on k.oid = t.typrelid and k.relkind <> 'c'
                where d.classid = 'pg_type'::regclass and t.oid = d.objid
                union all
                select
                    case when k.oid is not null then 'pg_class'::regclass::oid else 'pg_type'::regclass::oid end,
                    coalesce(k.oid, ta.typelem),
                    0
                from pg_type ta
                left join pg_type te on te.oid = ta.typelem
                left join pg_class k on k.oid = te.typrelid and k.relkind <> 'c'
                where d.classid = 'pg_type'::regclass and ta.oid = d.objid and ta.typcategory = 'A' and ta.typelem <> 0
            ) ma on true
        ) e
        cross join lateral (
            select coalesce(mb.toclassid, d.refclassid) as classid, coalesce(mb.toobjid, d.refobjid) as objid, coalesce(mb.tosubid, d.refobjsubid) as objsubid
            from (select) one
            left join lateral (
                select 'pg_class'::regclass::oid as toclassid, rw.ev_class as toobjid, 0 as tosubid
                from pg_rewrite rw
                where d.refclassid = 'pg_rewrite'::regclass and rw.oid = d.refobjid
                union all
                select 'pg_class'::regclass::oid, ad.adrelid, ad.adnum::int
                from pg_attrdef ad
                where d.refclassid = 'pg_attrdef'::regclass and ad.oid = d.refobjid
                union all
                select 'pg_class'::regclass::oid, k.oid, 0
                from pg_type t
                join pg_class k on k.oid = t.typrelid and k.relkind <> 'c'
                where d.refclassid = 'pg_type'::regclass and t.oid = d.refobjid
                union all
                select
                    case when k.oid is not null then 'pg_class'::regclass::oid else 'pg_type'::regclass::oid end,
                    coalesce(k.oid, ta.typelem),
                    0
                from pg_type ta
                left join pg_type te on te.oid = ta.typelem
                left join pg_class k on k.oid = te.typrelid and k.relkind <> 'c'
                where d.refclassid = 'pg_type'::regclass and ta.oid = d.refobjid and ta.typcategory = 'A' and ta.typelem <> 0
            ) mb on true
        ) f
        where d.deptype in ('n', 'a', 'i')
            and d.objid >= 16384
            and d.refobjid >= 16384
            and (f.classid, f.objid, f.objsubid) = (p.classid, p.objid, p.objsubid)
            and (e.classid, e.objid, e.objsubid) <> (f.classid, f.objid, f.objsubid)
    ) x
)
select
    coalesce(array_agg(s.classid::int8), '{}'),
    coalesce(array_agg(s.objid::int8), '{}'),
    coalesce(array_agg(s.objsubid::int4), '{}'),
    coalesce(array_agg(s.refclassid::int8), '{}'),
    coalesce(array_agg(s.refobjid::int8), '{}'),
    coalesce(array_agg(s.refobjsubid::int4), '{}')
from step s
where s.edge
