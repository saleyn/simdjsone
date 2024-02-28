%%%----------------------------------------------------------------------------
%%% @doc  Fast decoding of JSON using simdjson C++ library.
%%%
%%% By default JSON decoder uses the atom `null' to represent JSON nulls.
%%% To modify this behavior, set the following configuration option to another
%%% atom value (e.g. `nil' for Elixir):
%%% ```
%%% {simdjsone, [{null, nil}]}.
%%% '''
%%%
%%% See also [https://github.com/simdjson/simdjson]
%%% @end
%%%----------------------------------------------------------------------------
-module(simdjson).
-export([decode/1, decode/2, parse/1, get/2, get/3, minify/1, encode/1, encode/2]).
-export([int_to_bin/1]).

-compile({no_auto_import, [get/2]}).

-on_load(init/0).

-define(LIBNAME, simdjsone).
-define(NOT_LOADED_ERROR,
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})).

-type decode_opt() ::
  return_maps     |
  object_as_tuple |
  dedupe_keys     |
  use_nil         |
  {null_term, atom()}.

-type decode_opts() :: [decode_opt()].
%% Decode options:
%% <ul>
%% <li>`return_maps'     - decode JSON object as map</li>
%% <li>`object_as_tuple' - decode JSON object as a proplist wrapped in a tuple</li>
%% <li>`dedup_keys'      - eliminate duplicate keys from a JSON object</li>
%% <li>`use_nil'         - decode JSON "null" as `nil'</li>
%% <li>`{null_term, V}'  - use the given value `V' for a JSON "null"</li>
%% </ul>

-type encode_opt() ::
  uescape                |
  pretty                 |
  force_utf8             |
  use_nil                |
  escape_forward_slashes |
  {bytes_per_red, non_neg_integer()}.

-type encode_opts() :: [encode_opt()].
%% Encode options:
%% <ul>
%% <li>`uescape'            - escape UTF-8 sequences to produce a 7-bit clean output</li>
%% <li>`pretty'             - return JSON using two-space indentation</li>
%% <li>`use_nil'            - encode the atom `nil' as `null`</li>
%% <li>`escape_fwd_slash'   - escape the `/' character (useful when encoding URLs)</li>
%% <li>`{bytes_per_red, N}' - where `N' >= 0 - This controls the number of bytes
%% that Jiffy will process as an equivalent to a reduction. Each 20 reductions we
%% consume 1% of our allocated time slice for the current process. When the
%% Erlang VM indicates we need to return from the NIF.</li>
%% </ul>

-export_type([decode_opts/0, encode_opts/0]).

-ifdef(TEST).
-export([benchmark/1, benchmark/2]).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
  NullVal = application:get_env(simdjsone, null, null),
  is_atom(NullVal) orelse erlang:error("Option simdjsone/null must be an atom"),
  SoName  =
    case code:priv_dir(?LIBNAME) of
      {error, bad_name} ->
        case code:which(?MODULE) of
          Filename when is_list(Filename) ->
            Dir = filename:dirname(filename:dirname(Filename)),
            filename:join([Dir, "priv", ?LIBNAME]);
          _ ->
            filename:join("../priv", ?LIBNAME)
        end;
      Dir ->
        filename:join(Dir, ?LIBNAME)
  end,
  erlang:load_nif(SoName, [{null, NullVal}]).

%% @doc Decode a JSON string or binary to a term representation of JSON.
-spec decode(binary()|list()|reference()) -> term().
decode(_BinOrRef) ->
  ?NOT_LOADED_ERROR.

%% @doc Decode a JSON string or binary to a term representation of JSON.
-spec decode(binary()|list()|reference(), decode_opts()) -> term().
decode(_BinOrRef, _Opts) ->
  ?NOT_LOADED_ERROR.

%% @doc Parse a JSON string or binary and save it in a resource for later access by `get/2'.
%% Returns a resource reference owned by the calling pid.
-spec parse(binary()) -> reference().
parse(_Bin) ->
  ?NOT_LOADED_ERROR.

%% @doc Find a given `Path' (which must start with a slash) in the JSON resource.
%% The resource reference must have been previously created by calling
%% `parse/1,2'.
-spec get(reference(), binary()) -> term().
get(_Ref, Path) when is_binary(Path) ->
  ?NOT_LOADED_ERROR.

%% @doc Find a given `Path' (which must start with a slash) in the JSON resource.
%% The resource reference must have been previously created by calling
%% `parse/1,2'.
-spec get(reference(), binary(), decode_opts()) -> term().
get(_Ref, Path, _Opts) when is_binary(Path) ->
  ?NOT_LOADED_ERROR.

%% @doc Minify a JSON string or binary.
-spec minify(binary()|list()) -> {ok, binary()} | {error, binary()}.
minify(_BinOrStr) ->
  ?NOT_LOADED_ERROR.

%% @doc Encode a term to a JSON string.
-spec encode(term()) -> iodata().
encode(Data) ->
  encode(Data, []).

-spec encode(term(), encode_opts()) -> iodata().
encode(Data, Opts) ->
  ForceUTF8 = lists:member(force_utf8, Opts),
  encode_loop(fun() -> encode_init(Data, Opts) end, {ForceUTF8, Data, Opts}).

encode_loop(Fun, {ForceUTF8, _Data, _Opts} = State) ->
  case Fun() of
    {error, {invalid_string, _}} when ForceUTF8 ->
      {_ForceUTF8, Data, Opts} = State,
      FixedData = simdjson_utf8:fix(Data),
      encode(FixedData, Opts -- [force_utf8]);
    {error, {invalid_object_member_key, _}} when ForceUTF8 ->
      {_ForceUTF8, Data, Opts} = State,
      FixedData = simdjson_utf8:fix(Data),
      encode(FixedData, Opts -- [force_utf8]);
    {error, Error} ->
      error(Error);
    {partial, IOData} ->
      finish_encode(IOData, []);
    {iter, {NewEncoder, NewStack, NewIOBuf}} ->
      encode_loop(fun() -> encode_iter(NewEncoder, NewStack, NewIOBuf) end, State);
    [Bin] when is_binary(Bin) ->
      Bin;
    RevIOData when is_list(RevIOData) ->
      lists:reverse(RevIOData)
  end.

encode_init(_Data, _Opts) ->
  ?NOT_LOADED_ERROR.

encode_iter(_Encoder, _Stack, _IOBuf) ->
  ?NOT_LOADED_ERROR.

finish_encode([], Acc) ->
  %% No reverse! The NIF returned us
  %% the pieces in reverse order.
  Acc;
finish_encode([<<_/binary>>=B | Rest], Acc) ->
  finish_encode(Rest, [B | Acc]);
finish_encode([Val | Rest], Acc) when is_integer(Val) ->
  Bin = list_to_binary(integer_to_list(Val)),
  finish_encode(Rest, [Bin | Acc]);
finish_encode([InvalidEjson | _], _) ->
  error({invalid_ejson, InvalidEjson});
finish_encode(_, _) ->
  error(invalid_ejson).

%% @doc Fast integer to binary conversion
-spec int_to_bin(integer()) -> binary().
int_to_bin(_Int) ->
  ?NOT_LOADED_ERROR.

%%%----------------------------------------------------------------------------
%%% TEST
%%%----------------------------------------------------------------------------
-ifdef(EUNIT).

encode_test_() ->
  [
    ?_assertEqual(<<"null">>, encode(null)),
    ?_assertEqual(<<"null">>, encode(null, [use_nil])),
    ?_assertEqual(<<"null">>, encode(nil,  [use_nil])),
    ?_assertEqual(<<"{\"a\":1}">>, encode(#{a => 1})),
    ?_assertEqual(<<"[1000,\"a\"]">>, encode([1000, <<"a">>])),
    ?_assertEqual(<<"[]">>,     encode([])),

    fun() -> ok end
  ].

decode_test_() ->
  [
    ?_assertEqual([],                            decode("[]")),
    ?_assertEqual(null,                          decode("null")),
    ?_assertEqual(1,                             decode("1")),
    ?_assertEqual(1.0,                           decode("1.0")),
    ?_assertEqual(<<"abc">>,                     decode("\"abc\"")),
    ?_assertEqual(12345678901234567890123,       decode("12345678901234567890123")),
    ?_assertEqual([12312345123412341341234134,
                   234542345243524524352435243], simdjson:decode("[12312345123412341341234134,
                                                                   234542345243524524352435243]")),
    ?_assertEqual([1,2,3],                       decode("[1,2,3]")),
    ?_assertEqual(#{<<"a">> => 1,<<"b">> => 2},  decode("{\"a\": 1, \"b\": 2}")),
    ?_assertEqual({[{<<"a">>, 1},{<<"b">>, 2}]}, decode("{\"a\": 1, \"b\": 2}", [object_as_tuple])),
    ?_assertEqual({[{<<"a">>, 1},{<<"a">>, 2}]}, decode("{\"a\": 1, \"a\": 2}", [object_as_tuple])),
    ?_assertEqual({[{<<"a">>, 1}]},              decode("{\"a\": 1, \"a\": 2}", [object_as_tuple, dedupe_keys])),
    ?_assertEqual(#{<<"a">> => 1},               decode("{\"a\": 1, \"a\": 2}", [dedupe_keys])),
    ?_assertException(error, dup_keys_found,     decode("{\"a\": 1, \"a\": 2}")),
    ?_assertEqual(null,                          decode("null")),
    ?_assertEqual(nil,                           decode("null", [use_nil])),
    ?_assertEqual(null_atom,                     decode("null", [{null_term, null_atom}]))
  ].

cached_decode_test_() ->
  {setup,
    fun() ->
      parse("{\"a\": 1, \"b\": 2, \"c\": {\"a\": 1, \"b\": 2, \"c\": [1,2,3]}}")
    end,
    fun(Ref) ->
      [
        ?_assertEqual([1,2,3], get(Ref, "/c/c")),
        ?_assertEqual(1, get(Ref, "/c/c/0"))
      ]
    end
  }.

cached_decode_ownership_test_() ->
  {setup,
    fun() ->
      Pid = spawn(fun() -> receive {ready, P} -> P ! done end end),
      Ref = parse("{\"a\": 1, \"b\": 2, \"c\": {\"a\": 1, \"b\": 2, \"c\": [1,2,3]}}"),
      Pid ! {ready, self()},
      receive
        done -> Ref
      end
    end,
    fun(Ref) ->
      ?_assertEqual([1,2,3], get(Ref, "/c/c"))
      %%?_assertException(error, badarg, get(Ref, "/c/c"))
    end
  }.

minify_test_() ->
  [
    ?_assertEqual({ok, <<"[1,2,3]">>}, minify("[ 1, 2, 3 ]")),
    ?_assertEqual({ok, <<"[{\"a\":true,\"b\":false},2,3]">>}, minify("[ {\"a\": true, \"b\":  false}, 2, 3 ]"))
  ].

int_to_bin_test_() ->
  F = fun G(0, A) -> A;
          G(N, A) ->
            X = rand:uniform(1 bsl 60),
            case integer_to_binary(X) == int_to_bin(X) of
              true  -> G(N-1, A);
              false -> G(N-1, [X | A])
            end
      end,
  [
    ?_assertEqual([], F(1000000, []))
  ].

benchmark_test_() ->
  case os:getenv("MIX_ENV") of
    "test" ->
      ?_assert(true);
    _ ->
      ?_assertEqual(ok, benchmark([]))
  end.

benchmark(NameFuns) ->
  {ok, Dir} = file:get_cwd(),
  File1 = filename:join(Dir, "test/data/twitter.json"),
  benchmark(File1, NameFuns),
  File2 = filename:join(Dir, "test/data/esad.json"),
  benchmark(File2, NameFuns),
  File3 = filename:join(Dir, "test/data/small.json"),
  benchmark(File3, NameFuns),
  ok.

benchmark(File, NameFuns) ->
  {ok, Bin} = file:read_file(File),
  benchmark(100, Bin, NameFuns).

benchmark(N, Bin, NameFuns) ->
  erlang:group_leader(whereis(init), self()),
  io:format("\n=== Benchmark (file size: ~.1fK) ===\n", [byte_size(Bin) / 1024]),
  P = self(),
  L = [
    {"simdjsone", fun(B) -> simdjson:decode(B)             end},
    {"jiffy",     fun(B) -> jiffy:decode(B, [return_maps]) end},
    {"thoas",     fun(B) -> {ok, R} = thoas:decode(B),  R  end},
    {"euneus",    fun(B) -> {ok, R} = euneus:decode(B), R  end}
  ] ++ NameFuns,

  Tasks = [{Name, spawn(fun() ->
              P ! {Name, tc(N, fun() -> Fun(Bin) end)}
            end)} || {Name, Fun} <- L],

  K = length(Tasks),
  R = [receive Msg -> {ok, Msg} after 15000 -> {error, timeout} end || _ <- Tasks],
  M = lists:sort(fun({_, {T1, _}}, {_, {T2, _}}) -> T1 =< T2 end, [X || {ok, X} <- R]),
  [print(Nm, {T, S}) || {Nm, {T, S}} <- M],
  K = length(M),
  ok.

print(Fmt, {T, R}) ->
  case os:getenv("DEBUG") of
    V when V==false; V=="0" ->
      io:format("~12s: ~10.3fus\n", [Fmt, T]);
    _ ->
      io:format("~12s: ~10.3fus | Sample output: ~s\n", [Fmt, T, string:substr(lists:flatten(io_lib:format("~1024p", [R])), 1, 60)])
  end.

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

-endif.
