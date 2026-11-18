do $$ begin create role reader; exception when duplicate_object then null; end $$;
create table public.t (id int primary key);
