create table public.t (id int primary key);
create view public.v with (security_barrier) as select id from public.t;
