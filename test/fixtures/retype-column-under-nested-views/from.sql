do $x$ begin create role reader; exception when duplicate_object or unique_violation then null; end $x$;
create table public.t (id int primary key, amount int not null);
create view public.v as select id, amount from public.t;
create view public.w as select id, amount * 2 as doubled from public.v;
grant select on public.w to reader;
