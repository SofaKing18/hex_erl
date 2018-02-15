-module(hex_registry_tests).
-include_lib("eunit/include/eunit.hrl").

sanity_test() ->
    Packages = [#{name => <<"foo">>, repository => <<"hexpm">>}],
    Body = hex_registry:encode_names("hexpm", "dummy-signature", Packages),
    Packages = hex_registry:decode_names(Body),
    ok.
