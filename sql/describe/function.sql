select
    'pg_proc'::regclass::oid as classid,
    p.oid as objid,
    0 as objsubid,
    null::oid as relid,
    case
        when p.prokind = 'a' then (
            select concat_ws(
                E'\n',
                'aggregate kind ' || a.aggkind::text || ' direct args ' || a.aggnumdirectargs,
                'sfunc ' || a.aggtransfn::text,
                'stype ' || format_type(a.aggtranstype, null),
                'sspace ' || nullif(a.aggtransspace, 0),
                'finalfunc ' || nullif(a.aggfinalfn::oid, 0)::regproc::text,
                case when a.aggfinalextra then 'finalfunc_extra' end,
                'finalfunc_modify ' || a.aggfinalmodify::text,
                'combinefunc ' || nullif(a.aggcombinefn::oid, 0)::regproc::text,
                'serialfunc ' || nullif(a.aggserialfn::oid, 0)::regproc::text,
                'deserialfunc ' || nullif(a.aggdeserialfn::oid, 0)::regproc::text,
                'initcond ' || quote_literal(a.agginitval),
                'msfunc ' || nullif(a.aggmtransfn::oid, 0)::regproc::text,
                'minvfunc ' || nullif(a.aggminvtransfn::oid, 0)::regproc::text,
                'mstype ' || nullif(a.aggmtranstype, 0)::regtype::text,
                'mfinalfunc ' || nullif(a.aggmfinalfn::oid, 0)::regproc::text,
                'minitcond ' || quote_literal(a.aggminitval),
                'sortop ' || nullif(a.aggsortop, 0)::regoperator::text,
                'parallel ' || p.proparallel::text
            )
            from pg_aggregate a
            where a.aggfnoid = p.oid
        )
        else pg_get_functiondef(p.oid)
    end as definition
from pg_proc p
join pg_namespace n on n.oid = p.pronamespace
where n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
