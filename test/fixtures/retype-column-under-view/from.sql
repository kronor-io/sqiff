create table public.t (id int primary key, amount int not null);
create view public.v as select id, amount from public.t;
comment on view public.v is 'amounts';
