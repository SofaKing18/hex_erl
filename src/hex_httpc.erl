-module(hex_httpc).
-behavior(hex_http).
-export([get/1]).

get(Url) ->
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
    Body.
