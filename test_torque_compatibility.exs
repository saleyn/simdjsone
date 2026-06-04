# Test compatibility between simdjsone and torque
Mix.install([
  {:torque, "~> 0.1.9"}
])

defmodule CompatibilityTest do
  def run do
    # Ensure simdjsone is loaded
    :application.ensure_all_started(:simdjsone)

    test_cases = [
      "{\"key\":\"value\"}",
      "[1,2,3,\"test\"]",
      "{\"number\":42,\"float\":3.14,\"bool\":true,\"null\":null}",
      "{\"nested\":{\"array\":[1,{\"deep\":\"value\"}]}}",
      "{\"bigint\":12345678901234567890123}",
      "[]",
      "{}",
      "null",
      "true",
      "false",
      "42",
      "\"simple string\"",
      "{\"unicode\":\"Hello 世界 🌍\"}"
    ]

    IO.puts("Testing compatibility between simdjsone and torque...")

    Enum.each(test_cases, fn json_str ->
      test_case(json_str)
    end)

    IO.puts("\nCompatibility test completed!")
  end

  defp test_case(json_str) do
    try do
      simdjsone_result = :simdjson.decode(json_str)
      torque_result = Torque.decode(json_str)

      # Compare results (accounting for different representations)
      if results_equivalent?(simdjsone_result, torque_result) do
        IO.puts("✓ #{String.slice(json_str, 0, 50)}#{if String.length(json_str) > 50, do: "...", else: ""}")
      else
        IO.puts("✗ #{json_str}")
        IO.puts("  simdjsone: #{inspect(simdjsone_result)}")
        IO.puts("  torque:    #{inspect(torque_result)}")
      end
    rescue
      error ->
        IO.puts("✗ #{json_str} - Error: #{inspect(error)}")
    end
  end

  # Compare results allowing for different atom representations
  defp results_equivalent?(a, b) when a == b, do: true
  defp results_equivalent?(a, b) when is_map(a) and is_map(b) do
    Map.keys(a) == Map.keys(b) and
    Enum.all?(Map.keys(a), fn key ->
      results_equivalent?(Map.get(a, key), Map.get(b, key))
    end)
  end
  defp results_equivalent?(a, b) when is_list(a) and is_list(b) do
    length(a) == length(b) and
    Enum.zip(a, b) |> Enum.all?(fn {x, y} -> results_equivalent?(x, y) end)
  end
  # Handle null representation differences
  defp results_equivalent?(nil, :null), do: true
  defp results_equivalent?(:null, nil), do: true
  # Handle boolean representation differences
  defp results_equivalent?(true, :true), do: true
  defp results_equivalent?(:true, true), do: true
  defp results_equivalent?(false, :false), do: true
  defp results_equivalent?(:false, false), do: true
  defp results_equivalent?(_a, _b), do: false
end

CompatibilityTest.run()