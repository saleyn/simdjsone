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
$ make deps compile
```

**Elixir**
```bash
$ MIX_ENV=dev make deps compile
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
[jason](https://hex.pm/packages/jason) and
[poison](https://hex.pm/packages/poison) Elixir parsers.  The Elixir benchmarks
are more exhaustive:
```
$ MIX_ENV=test make benchmark
=== Benchmark (file size: 616.7K) ===

Name                ips        average  deviation         median         99th %
simdjsone        248.59        4.02 ms    ±18.96%        4.23 ms        6.12 ms
poison           156.55        6.39 ms    ±12.81%        6.16 ms       10.24 ms
jason            152.47        6.56 ms    ±10.07%        6.36 ms        9.37 ms
thaos            149.66        6.68 ms     ±7.21%        6.54 ms        8.86 ms
jiffy             77.59       12.89 ms    ±15.90%       12.65 ms       20.51 ms

Comparison:
simdjsone        248.59
poison           156.55 - 1.59x slower +2.37 ms
jason            152.47 - 1.63x slower +2.54 ms
thaos            149.66 - 1.66x slower +2.66 ms
jiffy             77.59 - 3.20x slower +8.87 ms

=== Benchmark (file size: 0.1K) ===

Name                ips        average  deviation         median         99th %
simdjsone      722.26 K        1.38 μs  ±2397.94%        1.10 μs        2.90 μs
poison         599.24 K        1.67 μs  ±1414.38%        1.40 μs        3.80 μs
thaos          500.98 K        2.00 μs   ±693.06%        1.70 μs        4.50 μs
jason          437.20 K        2.29 μs   ±809.21%           2 μs        5.80 μs
jiffy          315.95 K        3.17 μs   ±672.91%        2.60 μs        8.50 μs

Comparison:
simdjsone      722.26 K
poison         599.24 K - 1.21x slower +0.28 μs
thaos          500.98 K - 1.44x slower +0.61 μs
jason          437.20 K - 1.65x slower +0.90 μs
jiffy          315.95 K - 2.29x slower +1.78 μs
```

## TODO:

- Add support for `iterator`
- Add support for `iterate_many` and `parse_many`
