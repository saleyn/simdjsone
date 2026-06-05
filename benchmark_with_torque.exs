# Elixir script to benchmark JSON libraries including Torque
Mix.install([
  {:torque, "~> 0.1.9"},
  {:jiffy, "~> 1.1.1"},
  {:jason, "~> 1.4"},
  {:thoas, "~> 1.0"},
  {:euneus, "~> 2.0"},
  {:poison, "~> 6.0"},
  {:simdjsone, github: "saleyn/simdjsone", branch: "next"},
])

defmodule JSONBenchmark do
  def run do
    # Test data files
    test_files = [
      {"test/data/twitter.json", "Large"},
      {"test/data/esad.json", "Medium"},
      {"test/data/small.json", "Small"}
    ]

    Enum.each(test_files, fn {file, size_desc} ->
      if File.exists?(file) do
        run_benchmark_for_file(file, size_desc)
      else
        IO.puts("File #{file} not found, skipping...")
      end
    end)
  end

  def run_benchmark_for_file(file, size_desc) do
    {:ok, json_data} = File.read(file)
    file_size_kb = byte_size(json_data) / 1024

    IO.puts("\n=== Benchmark #{size_desc} (file size: #{:erlang.float_to_binary(file_size_kb, decimals: 1)}K) ===")

    # Libraries to benchmark
    libraries = [
      {"simdjsone", fn data -> :simdjson.decode(data) end},
      {"torque", fn data ->
        case Torque.decode(data) do
          {:ok, result} -> result
          error -> error
        end
      end},
      {"jason", fn data -> Jason.decode!(data) end},
      {"poison", fn data -> Poison.decode!(data) end},
      {"jiffy", fn data -> :jiffy.decode(data, [:return_maps]) end},
      {"thoas", fn data ->
        case :thoas.decode(data) do
          {:ok, result} -> result
          error -> error
        end
      end},
      {"euneus", fn data ->
        case :euneus.decode(data) do
          {:ok, result} -> result
          error -> error
        end
      end}
    ]

    # Benchmark each library
    results = Enum.map(libraries, fn {name, decode_fn} ->
      try do
        # Warmup
        decode_fn.(json_data)

        # Measure
        {time_us, _result} = :timer.tc(fn ->
          Enum.each(1..100, fn _ -> decode_fn.(json_data) end)
        end)

        avg_time_us = time_us / 100
        {name, avg_time_us, :ok}
      rescue
        error ->
          {name, :error, error}
      catch
        error ->
          {name, :error, error}
      end
    end)

    # Sort by performance and display
    successful_results =
      results
      |> Enum.filter(fn {_name, time, status} -> status == :ok and is_number(time) end)
      |> Enum.sort_by(fn {_name, time, _status} -> time end)

    Enum.each(successful_results, fn {name, time_us, _status} ->
      IO.puts("#{String.pad_trailing(name, 12)}: #{:erlang.float_to_binary(time_us, decimals: 3)}us")
    end)

    # Show errors if any
    error_results = Enum.filter(results, fn {_name, _time, status} -> status != :ok end)
    if length(error_results) > 0 do
      IO.puts("\nErrors:")
      Enum.each(error_results, fn {name, _time, error} ->
        IO.puts("  #{name}: #{inspect(error)}")
      end)
    end
  end
end

# Make sure simdjsone NIF is loaded
:application.ensure_all_started(:simdjsone)

# Run the benchmark
JSONBenchmark.run()

System.halt()
