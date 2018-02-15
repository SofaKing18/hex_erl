-module(hex_tarball_tests).
-include_lib("eunit/include/eunit.hrl").

sanity_test() ->
    Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Files = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files),
    {ok, {Metadata, Checksum, Files}} = hex_tarball:unpack(Tarball),
    ok.
