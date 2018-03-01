-module(hex_tarball_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

memory_test() ->
    Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Contents),
    {ok, #{checksum := Checksum, metadata := Metadata, contents := Contents}} = hex_tarball:unpack(Tarball, memory),
    ok.

disk_test() ->
    in_tmp(fun(Dir) ->
        ok = file:make_dir(Dir ++ "/pkg"),
        ok = file:write_file(Dir ++ "/pkg/foo.erl", <<"-module(foo).">>),

        Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
        Files = [{"foo.erl", Dir ++ "/pkg/foo.erl"}],
        {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files),
        {ok, #{checksum := Checksum, metadata := Metadata}} = hex_tarball:unpack(Tarball, Dir ++ "/unpack"),
        {ok, <<"-module(foo).">>} = file:read_file(Dir ++ "/unpack/foo.erl"),
        {ok, <<"{<<\"app\">>,<<\"foo\">>}.\n{<<\"version\">>,<<\"1.0.0\">>}.\n">>} =
            file:read_file(Dir ++ "/unpack/hex_metadata.config")
    end).

timestamps_and_permissions_test() ->
    in_tmp(fun(Dir) ->
        Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},

        ok = file:write_file(Dir ++ "/foo.sh", <<"">>),
        {ok, FileInfo} = file:read_file_info(Dir ++ "/foo.sh"),
        ok = file:write_file_info(Dir ++ "/foo.sh", FileInfo#file_info{mode=8#100755}),
        {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, [{"foo.erl", <<"">>}, {"foo.sh", Dir ++ "/foo.sh"}]),

        %% inside tarball
        {ok, Files} = hex_erl_tar:extract({binary, Tarball}, [memory]),
        {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, Files),
        {ok, [FooErlEntry, FooShEntry]} = hex_erl_tar:table({binary, ContentsBinary}, [compressed, verbose]),
        Epoch = epoch(),
        {_, regular, _, Epoch, 8#100644, 0, 0} = FooErlEntry,
        %% TODO: 0 should be Epoch
        {_, regular, _, 0, 8#100755, 0, 0} = FooShEntry,

        %% unpacked
        UnpackDir = Dir ++ "/timestamps_and_permissions",
        {ok, #{checksum := Checksum}} = hex_tarball:unpack(Tarball, UnpackDir),

        {ok, FooErlFileInfo} = file:read_file_info(UnpackDir ++ "/foo.erl"),
        {ok, FooShFileInfo} = file:read_file_info(UnpackDir ++ "/foo.sh"),
        8#100644 = FooErlFileInfo#file_info.mode,
        8#100755 = FooShFileInfo#file_info.mode,
        [{{2000,1,1}, {0,0,0}}] = calendar:local_time_to_universal_time_dst(FooErlFileInfo#file_info.mtime),
        %% TODO: should be either 2000,1,1 or current time
        [{{1970,1,1}, {0,0,0}}] = calendar:local_time_to_universal_time_dst(FooShFileInfo#file_info.mtime)
    end).

unpack_error_handling_test() ->
    {ok, {Tarball, Checksum}} = hex_tarball:create(#{"name" => <<"foo">>}, [{"rebar.config", <<"">>}]),
    {ok, #{checksum := Checksum}} = hex_tarball:unpack(Tarball, memory),
    {ok, OuterFileList} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    OuterFiles = maps:from_list(OuterFileList),

    %% tarball

    {error, {tarball, eof}} = hex_tarball:unpack(<<"badtar">>, memory),

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

%%====================================================================
%% Helpers
%%====================================================================

unpack_files(Files) ->
    FileList = maps:to_list(Files),
    ok = hex_erl_tar:create("test.tar", FileList, [write]),
    {ok, Binary} = file:read_file("test.tar"),
    hex_tarball:unpack(Binary, memory).

in_tmp(F) ->
    TmpDir = "tmp",
    ok = rm_rf(TmpDir),
    ok = file:make_dir(TmpDir),
    Dir = TmpDir ++ "/" ++ integer_to_list(erlang:unique_integer()),
    ok = file:make_dir(Dir),
    apply(F, [Dir]).

rm_rf(Path) ->
    [] = os:cmd("rm -rf " ++ Path),
    ok.

epoch() ->
    NixEpoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Y2kEpoch = calendar:datetime_to_gregorian_seconds({{2000, 1, 1}, {0, 0, 0}}),
    Y2kEpoch - NixEpoch.

