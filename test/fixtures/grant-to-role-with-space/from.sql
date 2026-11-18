do $$ begin create role "report readers"; exception when duplicate_object or unique_violation then null; end $$;
create table public.t (id int primary key);
