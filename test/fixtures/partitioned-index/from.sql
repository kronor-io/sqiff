create schema app;
create table app.events (id bigint not null, at date not null, note text) partition by range (at);
create table app.events_2026 partition of app.events for values from ('2026-01-01') to ('2027-01-01');
create table app.events_2027 partition of app.events for values from ('2027-01-01') to ('2028-01-01');
insert into app.events values (1, '2026-03-01', 'it''s 100%'), (2, '2027-03-01', null);
