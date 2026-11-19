select
    'pg_extension'::regclass::oid as classid,
    e.oid as objid,
    0 as objsubid,
    null::oid as relid,
    jsonb_build_object(
        'version', e.extversion,
        'schema', (select n.nspname from pg_namespace n where n.oid = e.extnamespace)
    )::text as definition
from pg_extension e
