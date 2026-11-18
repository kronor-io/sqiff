create schema app;
create table app.events (id bigint not null, at date not null) partition by range (at);
create table app.events_2026 partition of app.events for values from ('2026-01-01') to ('2027-01-01');
