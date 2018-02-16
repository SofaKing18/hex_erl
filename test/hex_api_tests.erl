-module(hex_api_tests).
-include_lib("eunit/include/eunit.hrl").

search_test() ->
    {ok, [First | _]} = hex_api:search(hexpm, "recon"),
    <<"https://hex.pm/api/packages/recon">> = maps:get(<<"url">>, First),
    ok.
