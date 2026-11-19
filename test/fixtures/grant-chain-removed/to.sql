do $x$ begin create role reader; exception when duplicate_object or unique_violation then null; end $x$;
do $x$ begin create role analyst; exception when duplicate_object or unique_violation then null; end $x$;
do $x$ begin create role intern; exception when duplicate_object or unique_violation then null; end $x$;
create table public.chain (id int primary key);
grant select on public.chain to reader;
