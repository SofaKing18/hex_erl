defmodule HexErl do
  use Mix.Project

  def project() do
    [
      app: :hex_erl,
      version: "0.1.0",
      language: :erlang
    ]
  end

  def application() do
    [
      extra_applications: [:inets, :ssl]
    ]
  end
end
