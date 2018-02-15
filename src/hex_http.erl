-module(hex_http).
-export([adapter/0, adapter/1, get/2, user_agent/0]).

-type url() :: string().
-type status_code() :: 200..599.
-type headers() :: [{string(), string()}].

-callback get(url(), string()) -> {ok, {status_code(), headers(), binary()}} | {error, term()}.

-callback user_agent_string() -> string().

-spec get(url(), string()) -> {ok, {status_code(), headers(), binary()}} | {error, term()}.
get(Url, Accept) ->
    Adapter = adapter(),
    Headers = [
           {"user-agent", user_agent(Adapter)},
           {"accept", Accept}
    ],
    Adapter:get(Url, Headers).

-spec adapter() -> module().
adapter() ->
    case application:get_env(hex_erl, http_adapter) of
        {ok, Adapter} -> Adapter;
        undefined -> hex_httpc
    end.

-spec adapter(module()) -> ok.
adapter(Adapter) when is_atom(Adapter) ->
    application:set_env(hex_erl, http_adapter, Adapter).

-spec user_agent() -> string().
user_agent() ->
    user_agent(adapter()).

user_agent(Adapter) ->
    OtpVersion = erlang:system_info(otp_release),
    UserAgentString = Adapter:user_agent_string(),
    "hex_erl/0.1.0 " ++ UserAgentString ++ " (OTP/" ++ OtpVersion ++ ")".
