# simdjsone

[![build](https://github.com/saleyn/simdjsone/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/simdjsone/actions/workflows/erlang.yml)

An implementation of the fastest JSON parser for Erlang/Elixir using the C++
[simdjson](https://github.com/simdjson/simdjson) NIF library. The decoding speed
of this parser is about 2.5 times faster than `jiffy`.

**NOTE**: The library currently doen't have a JSON encoder, it is only focused
on fast JSON parsing.

See [full documentation](https://simdjson.github.io/simdjson/index.html) of the C++ library.

Only a subset of functionality is implemented:

- Ability to decode JSON terms using a main scheduler or a dirty scheduler
  based on the size of JSON binary input.
- Ability to cache the decoded term, and access its key/value pairs using
  a NIF based `simdjson:get/2` function.
- The resources stored in the cache will get automatically garbage collected
  when the owner process dies.

For small JSON objects `simdjsone` is about twice faster than
[jiffy](https://github.com/davisp/jiffy) and for large JSON objects, it's about
30% faster than `jiffy`.

## Author

Serge Aleynikov

## Installation

Include the following dependency in your project.

Erlang (`rebar.config`):
```erlang
{deps, [{simdjsone, "0.1"}]}.
```

Elixir (`mix.exs`):
```elixir
def deps() do
  [{:simdjsone, "~> 0.1"}]
end
```

## Build

**Erlang:**
```bash
$ make deps
$ make
$ make test
```

**Elixir**
```bash
$ mix deps.get
$ mix compile
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

To run the performance benchmark of `simdjsone` against
[jiffy](https://hex.pm/packages/jiffy) and [thoas](https://hex.pm/packages/thoas)
do the following (prefix the command with `CXX=clang++` for using Clang C++
compiler):
```
$ make benchmark
=== Benchmark (file size: 616.7K) ===
   simdjsone:   5443.820us
       thoas:   9843.790us
       jiffy:  14222.450us

=== Benchmark (file size: 1.3K) ===
   simdjsone:      8.040us
       jiffy:     14.510us
       thoas:     21.630us

=== Benchmark (file size: 0.1K) ===
   simdjsone:      1.160us
       jiffy:      2.700us
       thoas:      2.510us
```
If you have Elixir installed, the project also includes a benchmark for the
[jason](https://hex.pm/packages/jason) Elixir parser.  The Elixir benchmarks
are more exhaustive, and test performance using 1, 4, and 8 parallel processes:
```
$ mix benchmark
=== Benchmark (file size: 616.7K) ===
   simdjsone:   5654.500us
       jason:   8745.330us
       thoas:   9052.840us
       jiffy:  13916.590us
=== Benchmark (file size: 1.3K) ===
   simdjsone:      8.680us
       jiffy:     14.490us
       thoas:     15.410us
       jason:     25.050us
```

## TODO:

- Add support for `iterator`
- Add support for `iterate_many` and `parse_many`
