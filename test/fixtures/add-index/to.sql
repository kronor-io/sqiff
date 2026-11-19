create table public.t (id int primary key, email text, deleted boolean);
create index t_email on public.t (lower(email)) where not deleted;
