-module(hex_repo_tests).
-include_lib("eunit/include/eunit.hrl").

names_test() ->
    [First | _] = hex_repo:names(hexpm),
    #{name := <<"a_message">>} = First,
    ok.

versions_test() ->
    [First | _] = hex_repo:versions(hexpm),
    #{name := <<"a_message">>, retired := [], versions := [<<"1.0.0">>]} = First,
    ok.

package_test() ->
    #{releases :=
      [#{checksum := _, dependencies := [], version := <<"1.0.0">>}]} = hex_repo:package(hexpm, "a_message"),
    ok.

tarball_test() ->
    <<_/binary>> = hex_repo:tarball(hexpm, "a_message", "1.0.0"),
    ok.
