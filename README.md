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
   simdjsone:   5539.670us
      euneus:   8435.540us
       thoas:   8902.160us
       jiffy:  13688.250us

=== Benchmark (file size: 1.3K) ===
   simdjsone:      8.030us
       jiffy:     14.950us
       thoas:     14.960us
      euneus:     19.830us

=== Benchmark (file size: 0.1K) ===
   simdjsone:      1.530us
       jiffy:      2.700us
      euneus:      3.220us
       thoas:      3.600us
```
If you have Elixir installed, the project also includes a benchmark for the
[jason](https://hex.pm/packages/jason) and
[poison](https://hex.pm/packages/poison) Elixir parsers.  The Elixir benchmarks
are more exhaustive:
```
$ MIX_ENV=test make benchmark
=== Benchmark (file size: 616.7K) ===

Name                ips        average  deviation         median         99th %
simdjsone        233.24        4.29 ms    ±32.31%        4.34 ms       10.19 ms
jason            160.87        6.22 ms     ±4.83%        6.14 ms        7.50 ms
poison           158.17        6.32 ms     ±7.06%        6.20 ms        8.67 ms
euneus           156.35        6.40 ms     ±8.01%        6.26 ms        8.47 ms
thaos            138.75        7.21 ms    ±15.95%        6.81 ms       11.96 ms
jiffy             83.03       12.04 ms     ±6.64%       12.10 ms       13.98 ms

Comparison:
simdjsone        233.24
jason            160.87 - 1.45x slower +1.93 ms
poison           158.17 - 1.47x slower +2.03 ms
euneus           156.35 - 1.49x slower +2.11 ms
thaos            138.75 - 1.68x slower +2.92 ms
jiffy             83.03 - 2.81x slower +7.76 ms

Memory usage statistics:

Name         Memory usage
simdjsone      0.00153 MB
jason             1.81 MB - 1188.30x memory usage +1.81 MB
poison            1.84 MB - 1206.10x memory usage +1.84 MB
euneus            1.87 MB - 1225.57x memory usage +1.87 MB
thaos             1.81 MB - 1188.19x memory usage +1.81 MB
jiffy             3.19 MB - 2088.37x memory usage +3.19 MB

**All measurements for memory usage were the same**

=== Benchmark (file size: 1.3K) ===

Name                ips        average  deviation         median         99th %
simdjsone      128.43 K        7.79 μs   ±468.75%        5.60 μs       21.10 μs
euneus         106.19 K        9.42 μs    ±87.91%        8.80 μs       21.40 μs
poison          97.80 K       10.23 μs    ±74.31%        9.30 μs          23 μs
jason           96.92 K       10.32 μs    ±98.77%        9.50 μs       26.30 μs
jiffy           90.31 K       11.07 μs    ±97.74%        9.20 μs       45.30 μs
thaos           79.04 K       12.65 μs   ±133.82%       11.50 μs       26.70 μs

Comparison:
simdjsone      128.43 K
euneus         106.19 K - 1.21x slower +1.63 μs
poison          97.80 K - 1.31x slower +2.44 μs
jason           96.92 K - 1.33x slower +2.53 μs
jiffy           90.31 K - 1.42x slower +3.29 μs
thaos           79.04 K - 1.62x slower +4.87 μs

Memory usage statistics:

Name         Memory usage
simdjsone         1.55 KB
euneus            5.22 KB - 3.36x memory usage +3.66 KB
poison            5.57 KB - 3.58x memory usage +4.02 KB
jason             5.29 KB - 3.40x memory usage +3.73 KB
jiffy             1.55 KB - 1.00x memory usage +0 KB
thaos             5.08 KB - 3.27x memory usage +3.52 KB

**All measurements for memory usage were the same**

=== Benchmark (file size: 0.1K) ===

Name                ips        average  deviation         median         99th %
simdjsone      695.44 K        1.44 μs  ±2134.49%        1.10 μs        3.40 μs
poison         613.54 K        1.63 μs  ±1373.87%        1.40 μs        3.60 μs
euneus         515.85 K        1.94 μs  ±1023.18%        1.70 μs        4.20 μs
thaos          497.72 K        2.01 μs   ±665.35%        1.80 μs        4.40 μs
jason          425.55 K        2.35 μs   ±816.21%           2 μs        6.10 μs
jiffy          327.46 K        3.05 μs   ±774.79%        2.50 μs        7.90 μs

Comparison:
simdjsone      695.44 K
poison         613.54 K - 1.13x slower +0.192 μs
euneus         515.85 K - 1.35x slower +0.50 μs
thaos          497.72 K - 1.40x slower +0.57 μs
jason          425.55 K - 1.63x slower +0.91 μs
jiffy          327.46 K - 2.12x slower +1.62 μs

Memory usage statistics:

Name         Memory usage
simdjsone         0.59 KB
poison            1.32 KB - 2.22x memory usage +0.73 KB
euneus            1.20 KB - 2.03x memory usage +0.61 KB
thaos             1.20 KB - 2.03x memory usage +0.61 KB
jason             1.27 KB - 2.14x memory usage +0.68 KB
jiffy             1.46 KB - 2.46x memory usage +0.87 KB
```

## TODO:

- Add support for `iterator`
- Add support for `iterate_many` and `parse_many`
