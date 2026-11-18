select
    'pg_class'::regclass::oid as classid,
    c.oid as objid,
    0 as objsubid,
    c.oid as relid,
    case
        when c.relkind in ('v', 'm') then
            jsonb_build_object(
                'kind', 'view',
                'options', c.reloptions,
                'query', pg_get_viewdef(c.oid)
            )
        else jsonb_build_object(
            'kind', 'table',
            'unlogged', c.relpersistence = 'u',
            'partition_by', case when c.relkind = 'p' then pg_get_partkeydef(c.oid) end,
            'partition_of', case
                when c.relispartition then (
                    select jsonb_build_object('schema', pn.nspname, 'name', p.relname, 'bound', pg_get_expr(c.relpartbound, c.oid))
                    from pg_inherits h
                    join pg_class p on p.oid = h.inhparent
                    join pg_namespace pn on pn.oid = p.relnamespace
                    where h.inhrelid = c.oid
                )
            end,
            'row_security', c.relrowsecurity,
            'force_row_security', c.relforcerowsecurity,
            -- The table's storage options, then its TOAST table's with a toast. prefix.
            'options', coalesce(c.reloptions, '{}') || array(select 'toast.' || o from unnest(toast.reloptions) o)
        )
    end::text as definition
from pg_class c
join pg_namespace n on n.oid = c.relnamespace
left join pg_class toast on toast.oid = c.reltoastrelid
where c.relkind in ('r', 'p', 'v', 'm')
    and ($2 or not c.relispartition)
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
