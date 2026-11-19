create table public.t (id int primary key);
create view public.v as select id from public.t;
