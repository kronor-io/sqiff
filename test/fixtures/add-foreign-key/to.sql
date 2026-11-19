create table public.parent (id int primary key);
create table public.child (id int primary key, parent_id int references public.parent (id) on delete cascade);
