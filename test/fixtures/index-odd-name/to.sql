create table public.t (id int primary key, note text);
create index "odd$$name%s" on public.t (note) where (note <> ' INDEX ' and note <> 'concurrently ');
comment on table public.t is 'a\b';
