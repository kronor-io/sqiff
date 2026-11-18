create table public.t (id int generated always as identity (start with 100 increment by 5 cache 10) primary key, a int);
