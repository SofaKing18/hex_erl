-module(hex_httpc).
-behavior(hex_http).
-export([get/1]).

get(Url) ->
    Headers = [
           {"user-agent", user_agent()},
           {"accept", "application/vnd.hex+erlang"}
    ],
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request(get, {Url, Headers}, [], [{body_format, binary}]),
    binary_to_term(Body).

user_agent() ->
    OtpVersion = erlang:system_info(otp_release),
    "hex_erl/0.1.0 (httpc) (OTP/" ++ OtpVersion ++ ")".
