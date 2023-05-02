defmodule Mix.Tasks.Bench do
  use Mix.Task

  def run(_) do
    Simdjsone.benchmark()
  end
end
