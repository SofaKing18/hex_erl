-module(hex_httpc).
-behavior(hex_http).
-export([get/2, user_agent_string/0]).

user_agent_string() ->
    "(httpc)".

get(Url, Headers) ->
    {ok, {{_, 200, _}, _Headers, Body}} =
        httpc:request(get, {Url, Headers}, [], [{body_format, binary}]),
    Body.
