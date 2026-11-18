create function public.greet(n text) returns text language plpgsql as $$ begin return 'hi ' || n; end $$;
