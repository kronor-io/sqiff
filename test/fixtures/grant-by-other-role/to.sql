do $x$ begin create role reader; exception when duplicate_object or unique_violation then null; end $x$;
do $x$ begin create role analyst; exception when duplicate_object or unique_violation then null; end $x$;
create table public.u (id int primary key);
grant select on public.u to reader with grant option;
set role reader;
grant select on public.u to analyst;
reset role;
