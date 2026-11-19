create schema app;
create function app.float_diff(a float8, b float8) returns float8 language sql immutable as 'select a - b';
create type app.span as range (subtype = float8, subtype_diff = app.float_diff, multirange_type_name = app.spans);
create type app.words as range (subtype = text, collation = "C");
