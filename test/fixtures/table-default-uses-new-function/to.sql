create schema app;
create function app.next_code() returns text language sql volatile as 'select md5(random()::text)';
create table app.voucher (id int primary key, code text not null default app.next_code());
