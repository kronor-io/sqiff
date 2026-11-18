create table public.t (id int primary key, a int);
create materialized view public.mv as select id, a from public.t;
create index mv_a on public.mv (a);
