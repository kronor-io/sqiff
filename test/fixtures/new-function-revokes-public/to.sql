create function public.secret() returns int language sql as $$ select 42 $$;
revoke execute on function public.secret() from public;
