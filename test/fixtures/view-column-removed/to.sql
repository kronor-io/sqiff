do $x$ begin create role reader; exception when duplicate_object then null; end $x$;
create table public.t (id int primary key, a int, b int);
create view public.v as select id, a from public.t;
comment on view public.v is 'the view';
comment on column public.v.a is 'column a';
grant select on public.v to reader;
create view public.uses_b as select id from public.v;
create view public.keeps as select id, a from public.v;
comment on view public.keeps is 'unchanged dependent';
