create function public.add(a int, b int) returns int language sql immutable as $$ select a + b + 0 $$;
