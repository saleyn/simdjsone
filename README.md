# esimdjson

[![build](https://github.com/saleyn/esimdjson/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/esimdjson/actions/workflows/erlang.yml)

An implementation of a fast Erlang JSON parser using the C++
[simdjson](https://github.com/simdjson/simdjson) library. The decoding speed of
this parser is about 2.5 times faster than `jiffy`.

See [full documentation](https://simdjson.github.io/simdjson/index.html) of the C++ library.

Only a subset of functionality is implemented:

- Ability to decode JSON terms using a main scheduler or a dirty scheduler
  based on the size of JSON binary input.
- Ability to cache the decoded term, and access its key/value pairs using
  a NIF based `get/2` function.
- The resources stored in the cache will get automatically garbage collected
  when the owner process dies.

For small JSON objects `esimdjson` is about twice faster than
[jiffy](https://github.com/davisp/jiffy) and for large JSON objects, it's about
30% faster than `jiffy`.

## Author

Serge Aleynikov

## Build

```bash
$ rebar3 get-deps
$ rebar3 compile
```

## Simple JSON decoding

```erlang
1> simdjson:decode("{\"a\": [1,2,3], \"b\": 123, \"c\": 12.234}").
#{<<"a">> => [1,2,3],<<"b">> => 123,<<"c">> => 12.234}
```

## Cached JSON decoding

After calling the `simdjson:parse/1`, the function `simdjson:get/2`
returns the value stored in a given path:

```erlang
1> Ref = simdjson:parse("{\"a\": [1,2,3], \"b\": 123, \"c\": 12.234}").
#Ref<0.1852532992.2458255361.217635>
2> simdjson:get(Ref, "/a").
[1,2,3]
3> simdjson:get(Ref, "/b").
123
4> simdjson:get(Ref, "/a/c").
12.234
ok
```

## Performance Benchmark

To run the performance benchmark of `esimdjson` against
[jiffy](https://hex.pm/packages/jiffy) and [thoas](https://hex.pm/packages/thoas) do:
```
$ make benchmark
=== Benchmark (file size: 616.7K) ===
   esimdjson:   4751.140us
       thoas:   8625.790us
       jiffy:  13075.610us

=== Benchmark (file size: 1.3K) ===
   esimdjson:      8.290us
       jiffy:     12.990us
       thoas:     23.830us
```
If you have Elixir install, the project also includes a benchmark for the
[jason](https://hex.pm/packages/jason) Elixir parser:
```
=== Benchmark (file size: 616.7K) ===
   esimdjson:   5654.500us
       jason:   8745.330us
       thoas:   9052.840us
       jiffy:  13916.590us
=== Benchmark (file size: 1.3K) ===
   esimdjson:      8.680us
       jiffy:     14.490us
       thoas:     15.410us
       jason:     25.050us
```

## TODO:

- Add support for `iterator`
- Add support for `iterate_many` and `parse_many`
