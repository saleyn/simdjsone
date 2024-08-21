# simdjsone

[![build](https://github.com/saleyn/simdjsone/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/simdjsone/actions/workflows/erlang.yml)

An implementation of the fastest JSON parser for Erlang/Elixir using the C++
[simdjson](https://github.com/simdjson/simdjson) NIF library. The decoding speed
of this parser is about 2.5 times faster than `jiffy`.

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

### Decoding

The following decoding options are supported in `decode(String, Options)`:

- `return_maps`        - decode JSON object as map (this is default)
- `object_as_tuple`    - decode JSON object as a proplist wrapped in a tuple
- `dedup_keys`         - eliminate duplicate keys from a JSON object
- `use_nil`            - decode JSON "null" as `nil`
- `{null_term, V}`     - use the given value `V` for a JSON "null"

### Encoding

The following encoding options are supported in `encode(String, Options)`:

- `uescape`            - escape UTF-8 sequences to produce a 7-bit clean output
- `pretty`             - return JSON using two-space indentation
- `use_nil`            - encode the atom `nil` as `null`
- `escape_fwd_slash`   - escape the `/` character (useful when encoding URLs)
- `{bytes_per_red, N}` - where `N` >= 0 - This controls the number of bytes
  that the NIF library will process as an equivalent to a reduction. Each 20
  reductions we consume 1% of our allocated time slice for the current process.
  When the Erlang VM indicates we need to return from the NIF.

**NOTE**: Since the simdjson library currently doen't have an implementation of
a JSON encoder, the encoding implementation is the jiffy's modified encoder
optimized for speed when encoding integers.

The implementation includes `simdjson:int_to_bin/1` function that is about 30%
faster than `erlang:integer_to_binary/1`, but it's limited to integers in range:
`(-1 bsl 63) <= I <= (1 bsl 62)`.

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
  [{:simdjsone, "~> 0.2"}]
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

## JSON encoding

```erlang
1> simdjson:encode(#{a => [1,2,3], <<"b">> => 123, c => 12.234}).
<<"{\"b\":123,\"a\":[1,2,3],\"c\":12.234}">>
2> simdjson:encode({[{a, [1,2,3]}, {<<"b">>, 123}, {c, 12.234}]}).
<<"{\"a\":[1,2,3],\"b\":123,\"c\":12.234}">>
```

## Performance Benchmark

To run the performance benchmark of `simdjsone` against
[jiffy](https://hex.pm/packages/jiffy) and [thoas](https://hex.pm/packages/thoas)
do the following (prefix the command with `CXX=clang++` for using Clang C++
compiler):
```
$ make benchmark
=== Benchmark (file size: 616.7K) ===
   simdjsone:   2526.420us
      euneus:   5314.820us
       thoas:   5452.380us
        json:   5541.130us
       jiffy:  10182.650us

=== Benchmark (file size: 1.3K) ===
   simdjsone:      6.940us
       thoas:      8.270us
      euneus:      9.790us
        json:     10.000us
       jiffy:     16.640us

=== Benchmark (file size: 0.1K) ===
   simdjsone:      1.930us
      euneus:      2.300us
        json:      2.500us
       jiffy:      2.770us
       thoas:      2.830us
```
If you have Elixir installed, the project also includes a benchmark for the
[jason](https://hex.pm/packages/jason) and
[poison](https://hex.pm/packages/poison) Elixir parsers.  The Elixir benchmarks
are more exhaustive:
```
$ MIX_ENV=test make benchmark
=== Benchmark (file size: 616.7K) ===

Name                ips        average  deviation         median         99th %
simdjsone        344.00        2.91 ms    ±18.13%        2.63 ms        4.82 ms
poison           154.88        6.46 ms    ±10.31%        6.25 ms        9.96 ms
jason            153.87        6.50 ms    ±11.37%        6.30 ms       10.18 ms
thaos            147.71        6.77 ms    ±11.53%        6.52 ms       10.64 ms
euneus           142.47        7.02 ms    ±22.71%        6.50 ms       13.47 ms
jiffy             78.74       12.70 ms    ±12.83%       12.52 ms       21.65 ms

Comparison:
simdjsone        344.00
poison           154.88 - 2.22x slower +3.55 ms
jason            153.87 - 2.24x slower +3.59 ms
thaos            147.71 - 2.33x slower +3.86 ms
euneus           142.47 - 2.41x slower +4.11 ms
jiffy             78.74 - 4.37x slower +9.79 ms

Memory usage statistics:

Name         Memory usage
simdjsone      0.00153 MB
poison            1.84 MB - 1200.09x memory usage +1.84 MB
jason             1.81 MB - 1182.38x memory usage +1.81 MB
thaos             1.81 MB - 1182.31x memory usage +1.81 MB
euneus            1.87 MB - 1219.47x memory usage +1.87 MB
jiffy             3.19 MB - 2077.98x memory usage +3.19 MB

**All measurements for memory usage were the same**

=== Benchmark (file size: 1.3K) ===

Name                ips        average  deviation         median         99th %
simdjsone      177.77 K        5.63 μs   ±187.84%        4.90 μs       16.90 μs
euneus         101.90 K        9.81 μs    ±94.81%           9 μs       22.30 μs
poison          98.29 K       10.17 μs    ±87.43%        9.40 μs       23.30 μs
jason           95.18 K       10.51 μs    ±99.89%        9.50 μs       30.70 μs
jiffy           88.49 K       11.30 μs   ±259.91%        9.30 μs       32.60 μs
thaos           84.36 K       11.85 μs    ±57.49%       10.90 μs       25.90 μs

Comparison:
simdjsone      177.77 K
euneus         101.90 K - 1.74x slower +4.19 μs
poison          98.29 K - 1.81x slower +4.55 μs
jason           95.18 K - 1.87x slower +4.88 μs
jiffy           88.49 K - 2.01x slower +5.68 μs
thaos           84.36 K - 2.11x slower +6.23 μs

Memory usage statistics:

Name         Memory usage
simdjsone         1.57 KB
euneus            5.22 KB - 3.32x memory usage +3.65 KB
poison            5.57 KB - 3.55x memory usage +4 KB
jason             5.29 KB - 3.37x memory usage +3.72 KB
jiffy             1.55 KB - 0.99x memory usage -0.01563 KB
thaos             5.22 KB - 3.32x memory usage +3.65 KB

**All measurements for memory usage were the same**

=== Benchmark (file size: 0.1K) ===

Name                ips        average  deviation         median         99th %
simdjsone      820.20 K        1.22 μs  ±1445.57%        1.10 μs        3.30 μs
poison         576.53 K        1.73 μs  ±1017.41%        1.40 μs        4.00 μs
thaos          489.07 K        2.04 μs  ±1003.95%        1.80 μs        4.70 μs
euneus         483.75 K        2.07 μs   ±624.18%        1.80 μs        4.70 μs
jason          435.99 K        2.29 μs   ±752.86%           2 μs        5.70 μs
jiffy          311.21 K        3.21 μs   ±652.57%        2.50 μs        9.10 μs

Comparison:
simdjsone      820.20 K
poison         576.53 K - 1.42x slower +0.52 μs
thaos          489.07 K - 1.68x slower +0.83 μs
euneus         483.75 K - 1.70x slower +0.85 μs
jason          435.99 K - 1.88x slower +1.07 μs
jiffy          311.21 K - 2.64x slower +1.99 μs

Memory usage statistics:

Name         Memory usage
simdjsone         0.50 KB
poison            1.32 KB - 2.64x memory usage +0.82 KB
thaos             1.20 KB - 2.41x memory usage +0.70 KB
euneus            1.20 KB - 2.41x memory usage +0.70 KB
jason             1.27 KB - 2.55x memory usage +0.77 KB
jiffy             1.46 KB - 2.92x memory usage +0.96 KB

**All measurements for memory usage were the same**
```

## TODO:

- Add support for `iterator`
- Add support for `iterate_many` and `parse_many`
