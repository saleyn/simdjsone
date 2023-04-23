#!/usr/bin/env escript
%% vim:ts=2:sw=2:et
%%! -noshell -noinput -env ERL_CRASH_DUMP /dev/null

-module(bench).
-export([main/1, test/2]).
-mode  (compile).


main([]) ->
  [code:add_patha(P) || P <- filelib:wildcard("_build/test/lib/*/ebin")],
  {ok, Bin} = file:read_file("test/data/twitter.json"),
  test(100, Bin),
  halt(0).

test(N, Bin) ->
  P = self(),
  Tasks = [
      spawn(fun() -> print("esimdjson", tc(N, fun() -> esimdjson:decode(Bin)            end)), P ! 1 end)
    , spawn(fun() -> print("jiffy",     tc(N, fun() -> jiffy:decode(Bin, [return_maps]) end)), P ! 1 end)
    , spawn(fun() -> print("thoas",     tc(N, fun() -> {ok, R} = thoas:decode(Bin), R   end)), P ! 1 end)
  ],
  K = length(Tasks),
  K = lists:sum([receive I -> I after 5000 -> timeout end || _ <- Tasks]),
  ok.

print(Fmt, {T, R}) ->
  io:format("~12s: ~10.3fus | Sample output: ~s\n", [Fmt, T, string:substr(lists:flatten(io_lib:format("~1024p", [R])), 1, 60)]).

tc(N, F) when N > 0 ->
  time_it(fun() -> exit(call(N, N, F, erlang:system_time(microsecond))) end).

time_it(F) ->
  Pid  = spawn_opt(F, [{min_heap_size, 16384}]),
  MRef = erlang:monitor(process, Pid),
  receive
  {'DOWN', MRef, process, _, Result} -> Result
  end.

call(1, X, F, Time1) ->
  Res = (catch F()),
  return(X, Res, Time1, erlang:system_time(microsecond));
call(N, X, F, Time1) ->
  (catch F()),
  call(N-1, X, F, Time1).

return(N, Res, Time1, Time2) ->
  Int   = Time2 - Time1,
  {Int / N, Res}.

