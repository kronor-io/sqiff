create table public.t (id int primary key, a int, b int);
create view public.v as select id, a from public.t;
