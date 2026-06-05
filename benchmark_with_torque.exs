# Elixir script to benchmark JSON libraries including Torque
# Run with: MIX_ENV=test make benchmark
#
# Environment variables to control simdjsone optimization level:
# SIMDJSONE_ORIGINAL=1   - Use original decoder (for comparison only)
# (no env var)           - Use Ultimate adaptive decoder (default, recommended)

optimization = cond do
  System.get_env("SIMDJSONE_ORIGINAL") == "1" -> "Original (for comparison)"
  true -> "Ultimate adaptive (default)"
end

IO.puts("🚀 Using simdjsone #{optimization}")

# Ensure applications are loaded
Application.ensure_all_started(:simdjsone)
Application.ensure_all_started(:jiffy)
Application.ensure_all_started(:thoas)
Application.ensure_all_started(:euneus)

# Try to start optional applications
try do
  Application.ensure_all_started(:torque)
rescue
  _ -> :ok
end

try do
  Application.ensure_all_started(:jason)
rescue
  _ -> :ok
end

try do
  Application.ensure_all_started(:poison)
rescue
  _ -> :ok
end

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

    # Determine current simdjsone optimization level for display
    optimization_level = cond do
      System.get_env("SIMDJSONE_ULTIMATE") == "1" -> "Ultimate"
      System.get_env("SIMDJSONE_PHASE3") == "1" -> "Phase3"
      System.get_env("SIMDJSONE_PHASE2") == "1" -> "Phase2"
      System.get_env("SIMDJSONE_OPTIMIZED") == "1" -> "Phase1"
      true -> "Original"
    end

    IO.puts("\n=== Benchmark #{size_desc} (file size: #{:erlang.float_to_binary(file_size_kb, decimals: 1)}K) ===")
    IO.puts("simdjsone optimization: #{optimization_level}")

    # Libraries to benchmark (with availability checks)
    all_libraries = [
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

    # Filter to only available libraries
    libraries = Enum.filter(all_libraries, fn {name, _} ->
      case name do
        "simdjsone" -> Code.ensure_loaded?(:simdjson)
        "torque" -> Code.ensure_loaded?(Torque)
        "jason" -> Code.ensure_loaded?(Jason)
        "poison" -> Code.ensure_loaded?(Poison)
        "jiffy" -> Code.ensure_loaded?(:jiffy)
        "thoas" -> Code.ensure_loaded?(:thoas)
        "euneus" -> Code.ensure_loaded?(:euneus)
        _ -> false
      end
    end)

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
