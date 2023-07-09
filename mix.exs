defmodule Simdjsone.MixProject do
  use Mix.Project

  def project do
    [
      app:             :simdjsone,
      version:         get_vsn(),
      elixir:          "~> 1.14",
      start_permanent: Mix.env() == :prod,
      elixirc_paths:   ["src"],
      compilers:       [:priv] ++ Mix.compilers,
      deps:            deps(),
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "~> 1.0",   only: :test},
      {:jiffy,   "~> 1.1.1", only: :test},
      {:jason,   "~> 1.4",   only: :test},
      {:thoas,   "~> 1.0",   only: :test},
      {:poison,  "~> 5.0",   only: :test},
    ]
  end

  defp get_vsn() do
    m =
      __MODULE__
      |> Atom.to_string()
      |> String.split(".")
      |> Enum.drop(1)
      |> Enum.take(1)
      |> hd()
      |> String.downcase

    "src/#{m}.app.src"
    |> :file.consult()
    |> elem(1)
    |> hd()
    |> elem(2)
    |> Keyword.get(:vsn)
    |> to_string()
  end
end

defmodule Mix.Tasks.Compile.Priv do
  @moduledoc """
  Compile the NIF library by running "make nif"
  """
  use Mix.Task.Compiler

  @impl true
  def run(_args) do
    outdir = Keyword.get(Mix.Project.config(), :app_path, File.cwd!)

    System.cmd("make", ["nif"], [env: [{"REBAR_BARE_COMPILER_OUTPUT_DIR", outdir}]])
    |> elem(0)
    |> IO.binwrite()
  end

  @impl true
  def clean() do
    System.cmd("make", ["clean"])
    |> elem(0)
    |> IO.binwrite()
  end
end

defmodule Mix.Tasks.Benchmark do
  use Mix.Task

  ## Execute Erlang unit tests followed by Elixir benchmarks
  @impl true
  def run(args) do
    System.cmd("make", ["test"])
    |> elem(0)
    |> IO.binwrite()

    Mix.env(:test)
    Mix.Task.run("test", args)
  end
end
