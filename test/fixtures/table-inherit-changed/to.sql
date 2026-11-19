create table public.base (id int not null);
create table public.audit (changed_at timestamptz);
create table public.child (id int not null, changed_at timestamptz, extra text) inherits (public.audit);
