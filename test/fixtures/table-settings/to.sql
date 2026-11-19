create unlogged table public.t (id int primary key) with (fillfactor = 80, autovacuum_enabled = false);
alter table public.t enable row level security;
alter table public.t force row level security;
