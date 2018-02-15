%% @private
-module(hex_httpc).
-behavior(hex_http).
-export([get/2, user_agent_string/0]).

user_agent_string() ->
    "(httpc)".

get(Url, Headers) ->
    {ok, {{_, StatusCode, _}, ResponseHeaders, ResponseBody}} =
        httpc:request(get, {Url, Headers}, [], [{body_format, binary}]),
    {ok, {StatusCode, ResponseHeaders, ResponseBody}}.
