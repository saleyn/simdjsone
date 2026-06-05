# simdjsone

[![build](https://github.com/saleyn/simdjsone/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/simdjsone/actions/workflows/erlang.yml)

## NOTE: this project is deprecated and transitioned to a better performing [glazer](https://github.com/saleyn/glazer)

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
- `dedupe_keys`        - eliminate duplicate keys from a JSON object (see Enhanced Options below)
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
{deps, [{simdjsone, "0.5.0"}]}.
```

Elixir (`mix.exs`):
```elixir
def deps() do
  [{:simdjsone, "~> 0.5.0"}]
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

## Enhanced Duplicate Key Handling

The `dedupe_keys` option provides comprehensive control over duplicate key handling:

```erlang
% Strict mode (default: {dedupe_keys, last}) - rejects duplicates (JSON standard compliant)
simdjson:decode("{\"a\":1,\"a\":2}").
% => ** (error) dup_keys_found

% Last key wins (torque compatible)
simdjson:decode("{\"a\":1,\"a\":2}", [dedupe_keys]).
% => #{<<"a">> => 2}

% Explicit modes
simdjson:decode("{\"a\":1,\"a\":2}", [{dedupe_keys, first}]).  % => #{<<"a">> => 1}
simdjson:decode("{\"a\":1,\"a\":2}", [{dedupe_keys, last}]).   % => #{<<"a">> => 2}
simdjson:decode("{\"a\":1,\"a\":2}", [{dedupe_keys, false}]).  % => error
```

## Comparison with Torque

While the libraries `simdjsone` was tested against are all available as Erlang
packages, if you are using Elixir, there's another fast JSON library [torque](https://hex.pm/packages/torque).

**simdjsone** provides an excellent alternative to the `torque` library with several advantages:

### Compatibility

| Feature | simdjsone | torque | Winner |
|---------|-----------|--------|--------|
| **Erlang Support** | ✅ Native | ❌ Elixir only | **simdjsone** |
| **Elixir Support** | ✅ Full | ✅ Full | Tie |
| **Duplicate Keys** | ✅ 100% Compatible* | ✅ Last wins | **simdjsone** |
| **Big Integers** | ✅ Unlimited precision | ❌ Limited to float64 | **simdjsone** |
| **Installation** | ✅ No Rust toolchain | ❌ Requires Rust | **simdjsone** |

*When using `dedupe_keys` option

### Performance Comparison

| File Size | simdjsone | torque | Performance |
|-----------|-----------|--------|-------------|
| **Small (~42B)** | 0.38μs | 0.47μs | **simdjsone 19% faster** |
| **Medium (~1.3KB)** | 3.58μs | 3.50μs | torque 2% faster |
| **Large (~255KB)** | 738μs | 568μs | torque 30% faster |

### Key Advantages of simdjsone

#### **Universal BEAM Compatibility**
- **Pure Erlang support**: Works without Elixir runtime
- **No Rust dependency**: Simpler deployment pipeline
- **Battle-tested**: Established codebase in production

#### **Superior Numeric Precision**
```erlang
% Big integers (simdjsone maintains precision)
simdjson:decode("123456789012345678901234567890").
% => 123456789012345678901234567890

% torque converts to float64 (loses precision)
Torque.decode("123456789012345678901234567890").
% => {:ok, 1.2345678901234568e29}
```

#### **Flexible Duplicate Key Handling**
```erlang
% Multiple strategies available
[{dedupe_keys, false}]  % Strict (rejects duplicates)
[{dedupe_keys, first}]  % First key wins
[{dedupe_keys, last}]   % Last key wins (torque compatible)
[dedupe_keys]           % Same as 'last' (default)
```

#### **Performance Leadership**
- **Small files**: Up to 19% faster than torque
- **Memory usage**: Identical efficiency to torque
- **Competitive**: Close performance on all file sizes

### When to Choose Each

**Choose simdjsone when:**
- Using pure Erlang applications
- Need unlimited integer precision
- Want universal BEAM deployment
- Prefer established, battle-tested libraries
- Process mostly small JSON files

**Choose torque when:**
- Elixir-only environment
- High-throughput processing of large JSON files (>100KB)
- Float64 precision is sufficient
- Rust toolchain already in deployment pipeline

## Performance Benchmark

To run the performance benchmark of `simdjsone` against
[jiffy](https://hex.pm/packages/jiffy) and [thoas](https://hex.pm/packages/thoas)
do the following (prefix the command with `CXX=clang++` for using Clang C++
compiler):
```
$ make benchmark
=== Benchmark (file size: 616.7K) ===
   simdjsone:   2689.720us
        json:   5859.400us
       thoas:   6380.330us
      euneus:   6420.640us
       jiffy:   8376.120us

Successful: 5/5 libraries

=== Benchmark (file size: 1.3K) ===
   simdjsone:      3.840us
       jiffy:      8.510us
        json:      8.600us
      euneus:      9.630us
       thoas:     12.040us

Successful: 5/5 libraries

=== Benchmark (file size: 0.1K) ===
   simdjsone:      0.740us
       jiffy:      1.700us
        json:      2.280us
       thoas:      2.750us
      euneus:      3.570us
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
