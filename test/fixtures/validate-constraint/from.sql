create table public.t (id int primary key, qty int);
alter table public.t add constraint qty_positive check (qty > 0) not valid;
