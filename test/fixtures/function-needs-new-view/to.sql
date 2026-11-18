create table public.t (id int primary key, a int);
create view public.v as select id, a from public.t;
create function public.total() returns bigint language sql stable as $$ select sum(a) from public.v $$;
