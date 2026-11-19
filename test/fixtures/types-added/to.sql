create schema app;
create type app.mood as enum ('it''s fine', 'ok');
create domain app.positive as int not null default 1;
create type app.pair as (x int, "Y" text);
create type app.span as range (subtype = float8);
