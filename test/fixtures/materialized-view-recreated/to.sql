create table public.t (id int primary key, a int);
create materialized view public.mv as select id, a, a * 2 as b from public.t;
create index mv_a on public.mv (a);
