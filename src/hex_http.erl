-module(hex_http).
-callback get(url()) -> binary().
-export([adapter/0, adapter/1, get/1]).

-type url() :: string().

-spec get(url()) -> binary().
get(Url) ->
    Adapter = adapter(),
    Adapter:get(Url).

adapter() ->
    case application:get_env(hex_erl, http_adapter) of
        {ok, Adapter} -> Adapter;
        undefined -> hex_httpc
    end.

adapter(Adapter) when is_atom(Adapter) ->
    application:set_env(hex_erl, http_adapter, Adapter).
