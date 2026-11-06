select
    'pg_class'::regclass::oid as classid,
    c.oid as objid,
    0 as objsubid,
    case
        when c.relkind in ('v', 'm') then
            concat_ws(
                E'\n',
                case when c.reloptions is not null then 'with (' || array_to_string(c.reloptions, ', ') || ')' end,
                pg_get_viewdef(c.oid)
            )
        else concat_ws(
            E'\n',
            case when c.relpersistence = 'u' then 'unlogged' end,
            case when c.relkind = 'p' then 'partition by ' || pg_get_partkeydef(c.oid) end,
            case
                when c.relispartition then
                    'partition of '
                    || (select h.inhparent::regclass::text from pg_inherits h where h.inhrelid = c.oid)
                    || ' '
                    || pg_get_expr(c.relpartbound, c.oid)
            end,
            case when c.relrowsecurity then 'row level security' end,
            case when c.relforcerowsecurity then 'force row level security' end,
            case
                when c.reloptions is not null or toast.reloptions is not null then
                    'with ('
                    || array_to_string(c.reloptions || array(select 'toast.' || o from unnest(toast.reloptions) o), ', ')
                    || ')'
            end
        )
    end as definition
from pg_class c
join pg_namespace n on n.oid = c.relnamespace
left join pg_class toast on toast.oid = c.reltoastrelid
where c.relkind in ('r', 'p', 'v', 'm')
    and n.nspname <> 'information_schema'
    and n.nspname !~ '^pg_'
