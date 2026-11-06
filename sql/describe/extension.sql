select
    'pg_extension'::regclass::oid as classid,
    e.oid as objid,
    0 as objsubid,
    null::oid as relid,
    'version ' || e.extversion || ' schema ' || e.extnamespace::regnamespace::text as definition
from pg_extension e
