-module(hex_tarball_tests).
-include_lib("eunit/include/eunit.hrl").

sanity_test() ->
    Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Files = #{"src/foo.erl" => <<"-module(foo).">>},
    {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files),
    {ok, {Metadata, Checksum, Files}} = hex_tarball:unpack(Tarball),
    ok.

unpack_error_handling_test() ->
    {ok, {Tar, Checksum}} = hex_tarball:create(#{"name" => <<"foo">>}, #{"rebar.config" => <<"">>}),
    {ok, {_, Checksum, _}} = hex_tarball:unpack(Tar),
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
      "CHECKSUM" => <<"1DF62C933B461E24877D01C0C923960D983981EB093F4EFC8D9CAC1A26675FF9">>
    },
    {error, {metadata, {illegal, "$"}}} = unpack_files(Files1),

    Files2 = OuterFiles#{
      "metadata.config" => <<"ok[">>,
      "CHECKSUM" => <<"F13D4C213ECBC7101A59D63077FB0E4A276409FE4E766B3D24C046BE051AABC2">>
    },
    {error, {metadata, invalid_terms}} = unpack_files(Files2),

    Files3 = OuterFiles#{
      "metadata.config" => <<"asdf.">>,
      "CHECKSUM" => <<"898A0FDF35F59DC0CEDFCFE8FB7FC9EE4BCB82FF3BE64AB6152BB2567F30DF5A">>
    },
    {error, {metadata, {user, "illegal atom asdf"}}} = unpack_files(Files3),

    Files4 = OuterFiles#{
      "metadata.config" => <<"ok.">>,
      "CHECKSUM" => <<"6D6CD6DCABA57CBCBBE0E602629E3AA1605F11B26EA79BECE2DC92FFDF500E2D">>
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
