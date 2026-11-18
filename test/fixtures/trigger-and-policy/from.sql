create table public.t (id int primary key, owner text, touched timestamptz);
create function public.touch() returns trigger language plpgsql as $$ begin new.touched := now(); return new; end $$;
