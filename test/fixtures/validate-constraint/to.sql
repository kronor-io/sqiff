create table public.t (id int primary key, qty int, constraint qty_positive check (qty > 0));
