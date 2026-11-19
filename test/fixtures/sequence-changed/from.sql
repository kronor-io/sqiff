create table public.t (id int);
create sequence public.counter owned by public.t.id;
