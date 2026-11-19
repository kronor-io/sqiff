create schema app;
create table app.account (
    id bigint generated always as identity primary key,
    email text not null unique,
    created_at timestamptz not null default now()
);
