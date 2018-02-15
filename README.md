# hex_erl

## Usage

Get package names:

```erlang
Packages = hex_repo:names(hexpm),
length(Packages) >= 5805.
%%=> true

hd(Packages).
%%=> #{name => <<"a_message">>}
```

Get package versions:

```erlang
Versions = hex_repo:versions(hexpm),
length(Versions) >= 5805.
%%=> true

hd(Versions).
%%=> #{name => <<"a_message">>, retired => [], versions => [<<"1.0.0">>]}
```

Get package info:

```erlang
hex_repo:package(hexpm, "a_message").
%%=> #{releases =>
%%=>       [#{checksum =>
%%=>              <<74,15,232,35,81,34,183,47,130,224,43,1,253,91,75,134,
%%=>                111,243,57,3,52,23,216,95,240,182,...>>,
%%=>          dependencies => [],version => <<"1.0.0">>}]}
```

Get package tarball:

```erlang
Tarball = hex_repo:tarball(hexpm, "a_message", "1.0.0").
%%=> <<86,69,...>>
```

Unpack package tarball:

```erlang
hex_tarball:unpack(Tarball).
%%=> {ok,{#{<<"app">> => <<"a_message">>,
%%=>        <<"build_tools">> => [<<"mix">>],
%%=>        ...}}}
```

Create tarball:

```erlang
Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
Files = [{"src/foo.erl", <<"-module(foo).">>}],
{ok, {Tarball2, Checksum2}} = hex_tarball:create(Metadata, Files).
Tarball2.
%%=> <<86,69,...>>
Checksum2.
%%=> <<40,32,...>>
```

Searching for packages:

```erlang
hex_api:search(hexpm, "recon").
%%=> [#{<<"downloads">> => ...,
%%=>    <<"html_url">> => <<"https://hex.pm/packages/recon">>,
%%=>    <<"meta">> =>
%%=>        #{<<"description">> => <<"Diagnostic tools for production use">>,
%%=>          <<"licenses">> => [<<"BSD">>],
%%=>          <<"links">> =>
%%=>              #{<<"Documentation">> => <<"http://ferd.github.io/recon/">>,
%%=>                <<"Github">> => <<"https://github.com/ferd/recon/">>},
%%=>          <<"maintainers">> => [<<"Fred Hebert">>]},
%%=>    <<"name">> => <<"recon">>,
%%=>    ...},
%%=>   ...]
```

## License

Apache 2.0
