create table public.t (id int primary key, owner text, touched timestamptz);
create function public.touch() returns trigger language plpgsql as $$ begin new.touched := now(); return new; end $$;
create trigger t_touch before update on public.t for each row execute function public.touch();
alter table public.t enable row level security;
create policy own on public.t using (owner = current_user);
