create table public.t (id int primary key, a int);
comment on table public.t is 'new';
comment on column public.t.a is 'the a';
