create table public.base (id int not null, made timestamptz not null default now(), constraint base_id_positive check (id > 0));
create table public.other_base (note text);
