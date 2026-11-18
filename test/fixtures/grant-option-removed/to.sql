do $x$ begin create role reader; exception when duplicate_object then null; end $x$;
do $x$ begin create role analyst; exception when duplicate_object then null; end $x$;
create table public.t (id int primary key);
grant select on public.t to reader;
