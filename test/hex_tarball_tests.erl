-module(hex_tarball_tests).
-include_lib("eunit/include/eunit.hrl").

sanity_test() ->
    Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Files = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files),
    {ok, #{checksum := Checksum, metadata := Metadata, files := Files}} = hex_tarball:unpack(Tarball),
    ok.

unpack_error_handling_test() ->
    {ok, {Tar, Checksum}} = hex_tarball:create(#{"name" => <<"foo">>}, [{"rebar.config", <<"">>}]),
    {ok, #{checksum := Checksum}} = hex_tarball:unpack(Tar),
    {ok, OuterFileList} = hex_erl_tar:extract({binary, Tar}, [memory]),
    OuterFiles = maps:from_list(OuterFileList),

    %% tarball

    {error, {tarball, eof}} = hex_tarball:unpack(<<"badtar">>),

    {error, {tarball, empty}} = unpack_files(#{}),

    {error, {tarball, {missing_files, ["VERSION", "CHECKSUM"]}}} =
        unpack_files(#{"metadata.config" => <<"">>, "contents.tar.gz" => <<"">>}),

    {error, {tarball, {invalid_files, ["invalid.txt"]}}} =
        unpack_files(#{"invalid.txt" => <<"">>}),

    {error, {tarball, {bad_version, <<"1">>}}} =
        unpack_files(OuterFiles#{"VERSION" => <<"1">>}),

    {error, {tarball, invalid_checksum}} =
        unpack_files(OuterFiles#{"CHECKSUM" => <<"bad">>}),

    {error, {tarball, {checksum_mismatch, _, _}}} =
        unpack_files(OuterFiles#{"contents.tar.gz" => <<"">>}),

    %% metadata

    Files1 = OuterFiles#{
      "metadata.config" => <<"ok $">>,
      "CHECKSUM" => <<"1BB37F9A91F9E4A3667A4527930187ACF6B9714C0DE7EADD55DC31BE5CFDD98C">>
    },
    {error, {metadata, {illegal, "$"}}} = unpack_files(Files1),

    Files2 = OuterFiles#{
      "metadata.config" => <<"ok[">>,
      "CHECKSUM" => <<"0423D201115A49644F8BD4F216E666AF823CFE759853D9994CE9B652C5E604D9">>
    },
    {error, {metadata, invalid_terms}} = unpack_files(Files2),

    Files3 = OuterFiles#{
      "metadata.config" => <<"asdf.">>,
      "CHECKSUM" => <<"F80D9B63D52695C6AC165D41F0F42F8D19152BE84D11FC2E5093FEC53CD4E3D9">>
    },
    {error, {metadata, {user, "illegal atom asdf"}}} = unpack_files(Files3),

    Files4 = OuterFiles#{
      "metadata.config" => <<"ok.">>,
      "CHECKSUM" => <<"5E891D99F011F3DF8AB42E3B16420034C63CEC771B548F5E430057D13B62EF2B">>
    },
    {error, {metadata, not_key_value}} = unpack_files(Files4),

    %% contents

    Files5 = OuterFiles#{
      "contents.tar.gz" => <<"badtar">>,
      "CHECKSUM" => <<"77D5649A97731EE82A736E1E16E58094A61BFD38A28D4B6CD9EEAC527D3C4C08">>
    },
    {error,{inner_tarball,eof}} = unpack_files(Files5),

    ok.

unpack_files(Files) ->
    FileList = maps:to_list(Files),
    ok = hex_erl_tar:create("test.tar", FileList, [write]),
    {ok, Binary} = file:read_file("test.tar"),
    hex_tarball:unpack(Binary).
