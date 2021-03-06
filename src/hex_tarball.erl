%% based on:
%%   https://github.com/hexpm/hex/blob/master/lib/hex/tar.ex
%%   https://github.com/tsloughter/rebar3_hex/blob/master/src/rebar3_hex_tar.erl
-module(hex_tarball).
-export([create/2, unpack/2, format_checksum/1, format_error/1, gzip/1]).
-define(VERSION, <<"3">>).
-define(TARBALL_MAX_SIZE, 8 * 1024 * 1024).
-include_lib("kernel/include/file.hrl").

-type checksum() :: binary().
-type contents() :: #{filename() => binary()}.
-type filename() :: string().
-type files() :: [filename()] | contents().
-type metadata() :: map().
-type tarball() :: binary().

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Creates a package tarball.
%%
%% Examples:
%%
%% ```
%%     Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
%%     Files = [{"src/foo.erl", <<"-module(foo).">>}],
%%     {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files).
%%     Tarball.
%%     %%=> <<86,69,...>>
%%     Checksum2.
%%     %%=> <<40,32,...>>
%% '''
%% @end
-spec create(metadata(), files()) -> {ok, {tarball(), checksum()}}.
create(Metadata, Files) ->
    MetadataBinary = encode_metadata(Metadata),
    ContentsBinary = create_tarball(Files, [compressed]),
    Checksum = checksum(?VERSION, MetadataBinary, ContentsBinary),
    ChecksumBase16 = encode_base16(Checksum),

    OuterFiles = [
      {"VERSION", ?VERSION},
      {"CHECKSUM", ChecksumBase16},
      {"metadata.config", MetadataBinary},
      {"contents.tar.gz", ContentsBinary}
    ],

    Tarball = create_tarball(OuterFiles, []),

    case byte_size(Tarball) > ?TARBALL_MAX_SIZE of
        true ->
            {error, {tarball, too_big}};

        false ->
            {ok, {Tarball, Checksum}}
    end.

%% @doc
%% Unpacks a package tarball.
%%
%% Examples:
%%
%% ```
%%     hex_tarball:unpack(Tarball, memory).
%%     %%=> {ok,#{checksum => <<40,32,...>>,
%%     %%=>       contents => [{"src/foo.erl",<<"-module(foo).">>}],
%%     %%=>       metadata => #{<<"app">> => <<"foo">>, ...}}}
%%
%%     hex_tarball:unpack(Tarball, "path/to/unpack").
%%     %%=> {ok,#{checksum => <<40,32,...>>,
%%     %%=>       metadata => #{<<"app">> => <<"foo">>, ...}}}
%% '''
-spec unpack(tarball(), memory) ->
                {ok, #{checksum => checksum(), metadata => metadata(), contents => contents()}} |
                {error, term()};
            (tarball(), filename()) ->
                {ok, #{checksum => checksum(), metadata => metadata()}} |
                {error, term()}.
unpack(Tarball, _) when byte_size(Tarball) > ?TARBALL_MAX_SIZE ->
    {error, {tarball, too_big}};

unpack(Tarball, Output) ->
    case hex_erl_tar:extract({binary, Tarball}, [memory]) of
        {ok, []} ->
            {error, {tarball, empty}};

        {ok, Files} ->
            start_unpack(#{
                checksum => nil,
                contents => nil,
                files => maps:from_list(Files),
                metadata => nil,
                output => Output
            });

        {error, Reason} ->
            {error, {tarball, Reason}}
    end.

%% @doc
%% Returns base16-encoded representation of checksum.
-spec format_checksum(checksum()) -> string().
format_checksum(Checksum) ->
    encode_base16(Checksum).

%% @doc
%% Converts an error reason term to a human-readable error message string.
-spec format_error(term()) -> string().
format_error({tarball, empty}) -> "empty tarball";
format_error({tarball, too_big}) -> "tarball is too big";
format_error({tarball, {missing_files, Files}}) -> io_lib:format("missing files: ~p", [Files]);
format_error({tarball, {invalid_files, Files}}) -> io_lib:format("invalid files: ~p", [Files]);
format_error({tarball, {bad_version, Vsn}}) -> io_lib:format("unsupported version: ~p", [Vsn]);
format_error({tarball, invalid_checksum}) -> "invalid tarball checksum";
format_error({tarball, Reason}) -> "tarball error, " ++ hex_erl_tar:format_error(Reason);
format_error({inner_tarball, Reason}) -> "inner tarball error, " ++ hex_erl_tar:format_error(Reason);
format_error({metadata, invalid_terms}) -> "error reading package metadata: invalid terms";
format_error({metadata, not_key_value}) -> "error reading package metadata: not in key-value format";
format_error({metadata, Reason}) -> "error reading package metadata" ++ safe_erl_term:format_error(Reason);

format_error({checksum_mismatch, ExpectedChecksum, ActualChecksum}) ->
    io_lib:format(
        "tarball checksum mismatch~n~n" ++
        "Expected (base16-encoded): ~s~n" ++
        "Actual   (base16-encoded): ~s",
        [encode_base16(ExpectedChecksum), encode_base16(ActualChecksum)]).

%%====================================================================
%% Internal functions
%%====================================================================

checksum(Version, MetadataBinary, ContentsBinary) ->
    Blob = <<Version/binary, MetadataBinary/binary, ContentsBinary/binary>>,
    crypto:hash(sha256, Blob).

encode_metadata(Meta) ->
    Data = lists:map(fun(MetaPair) ->
        String = io_lib_pretty:print(binarify(MetaPair), [{encoding, utf8}]),
        unicode:characters_to_binary([String, ".\n"])
      end, maps:to_list(Meta)),
    iolist_to_binary(Data).

start_unpack(State) ->
    State1 = check_files(State),
    State2 = check_version(State1),
    State3 = check_checksum(State2),
    State4 = decode_metadata(State3),
    finish_unpack(State4).

finish_unpack({error, _} = Error) ->
    Error;
finish_unpack(#{metadata := Metadata, files := Files, output := Output}) ->
    _Version = maps:get("VERSION", Files),
    Checksum = decode_base16(maps:get("CHECKSUM", Files)),
    ContentsBinary = maps:get("contents.tar.gz", Files),
    case unpack_tarball(ContentsBinary, Output) of
        ok ->
            copy_metadata_config(Output, maps:get("metadata.config", Files)),
            {ok, #{checksum => Checksum, metadata => Metadata}};

        {ok, Contents} ->
            {ok, #{checksum => Checksum, metadata => Metadata, contents => Contents}};

        {error, Reason} ->
            {error, {inner_tarball, Reason}}
    end.

copy_metadata_config(Output, MetadataBinary) ->
    ok = file:write_file(Output ++ "/hex_metadata.config", MetadataBinary).

check_files({error, _} = Error) ->
    Error;
check_files(#{files := Files} = State) ->
    RequiredFiles = ["VERSION", "CHECKSUM", "metadata.config", "contents.tar.gz"],
    case diff_keys(Files, RequiredFiles, []) of
        ok ->
            State;

        {error, {missing_keys, Keys}} ->
            {error, {tarball, {missing_files, Keys}}};

        {error, {unknown_keys, Keys}} ->
            {error, {tarball, {invalid_files, Keys}}}
    end.

check_version({error, _} = Error) ->
    Error;
check_version(#{files := Files} = State) ->
    case maps:get("VERSION", Files) of
        <<"3">> ->
            State;

        Version ->
            {error, {tarball, {bad_version, Version}}}
    end.

check_checksum({error, _} = Error) ->
    Error;
check_checksum(#{files := Files} = State) ->
    ChecksumBase16 = maps:get("CHECKSUM", Files),
    ExpectedChecksum = decode_base16(ChecksumBase16),

    Version = maps:get("VERSION", Files),
    MetadataBinary = maps:get("metadata.config", Files),
    ContentsBinary = maps:get("contents.tar.gz", Files),
    ActualChecksum = checksum(Version, MetadataBinary, ContentsBinary),

    if
        byte_size(ExpectedChecksum) /= 32 ->
            {error, {tarball, invalid_checksum}};

        ExpectedChecksum == ActualChecksum ->
            maps:put(checksum, ExpectedChecksum, State);

        true ->
            {error, {tarball, {checksum_mismatch, ExpectedChecksum, format_checksum(ActualChecksum)}}}
    end.

decode_metadata({error, _} = Error) ->
    Error;
decode_metadata(#{files := #{"metadata.config" := Binary}} = State) when is_binary(Binary) ->
    String = binary_to_list(Binary),
    case safe_erl_term:string(String) of
        {ok, Tokens, _Line} ->
            try
                Terms = safe_erl_term:terms(Tokens),
                maps:put(metadata, normalize_metadata(Terms), State)
            catch
                error:function_clause ->
                    {error, {metadata, invalid_terms}};

                error:badarg ->
                    {error, {metadata, not_key_value}}
            end;

        {error, {_Line, safe_erl_term, Reason}, _Line2} ->
            {error, {metadata, Reason}}
    end.

normalize_metadata(Terms) ->
    Metadata = maps:from_list(Terms),
    maybe_update_with(<<"requirements">>, fun normalize_requirements/1, Metadata).

normalize_requirements(Requirements) ->
    case is_list(Requirements) andalso (Requirements /= []) andalso is_list(hd(Requirements)) of
        true ->
            try_into_map(fun normalize_requirement/1, Requirements);

        false ->
            try_into_map(fun({Key, Value}) ->
                                 {Key, try_into_map(fun(X) -> X end, Value)} end,
                         Requirements)
    end.

normalize_requirement(Requirement) ->
    {_, Name} = lists:keyfind(<<"name">>, 1, Requirement),
    List = lists:keydelete(<<"name">>, 1, Requirement),
    {Name, maps:from_list(List)}.

%%====================================================================
%% Tar Helpers
%%====================================================================

unpack_tarball(ContentsBinary, memory) ->
    hex_erl_tar:extract({binary, ContentsBinary}, [memory, compressed]);
unpack_tarball(ContentsBinary, Output) ->
    hex_erl_tar:extract({binary, ContentsBinary}, [{cwd, Output}, compressed]).

create_tarball(Files, Options) ->
    Tarball = create_memory_tarball(Files),
    case proplists:get_bool(compressed, Options) of
        true -> gzip(Tarball);
        false -> Tarball
    end.

create_memory_tarball(Files) ->
    {ok, Fd} = file:open([], [ram, read, write, binary]),
    {ok, Tar} = hex_erl_tar:init(Fd, write, fun file_op/2),

    try
        try
            add_files(Tar, Files)
        after
            ok = hex_erl_tar:close(Tar)
        end,

        {ok, Size} = file:position(Fd, cur),
        {ok, Binary} = file:pread(Fd, 0, Size),
        Binary
    after
        ok = file:close(Fd)
    end.

file_op(write, {Fd, Data}) -> file:write(Fd, Data);
file_op(position, {Fd, Pos}) -> file:position(Fd, Pos);
file_op(read2, {Fd, Size}) -> file:read(Fd, Size);
file_op(close, _Fd) -> ok.

add_files(Tar, Files) when is_map(Files) ->
    maps:map(fun(Filename, Binary) -> add_file(Tar, {Filename, Binary}) end, Files);
add_files(Tar, Files) when is_list(Files) ->
    lists:map(fun(File) -> add_file(Tar, File) end, Files).

add_file(Tar, {Filename, Contents}) when is_list(Filename) and is_binary(Contents) ->
    ok = hex_erl_tar:add(Tar, Contents, Filename, tar_opts());
add_file(Tar, Filename) when is_list(Filename) ->
    {ok, FileInfo} = file:read_link_info(Filename, []),

    case FileInfo#file_info.type of
        symlink ->
            ok = hex_erl_tar:add(Tar, Filename, tar_opts());
        directory ->
            ok = hex_erl_tar:add(Tar, Filename, tar_opts());
        _ ->
            Mode = FileInfo#file_info.mode,
            {ok, Contents} = file:read_file(Filename),
            ok = hex_erl_tar:add(Tar, Contents, Filename, Mode, tar_opts())
    end.

tar_opts() ->
    NixEpoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Y2kEpoch = calendar:datetime_to_gregorian_seconds({{2000, 1, 1}, {0, 0, 0}}),
    Epoch = Y2kEpoch - NixEpoch,
    [{atime, Epoch}, {mtime, Epoch}, {ctime, Epoch}, {uid, 0}, {gid, 0}].

%% Reproducible gzip by not setting mtime and OS
%%
%% From https://tools.ietf.org/html/rfc1952
%%
%% +---+---+---+---+---+---+---+---+---+---+
%% |ID1|ID2|CM |FLG|     MTIME     |XFL|OS | (more-->)
%% +---+---+---+---+---+---+---+---+---+---+
%%
%% +=======================+
%% |...compressed blocks...| (more-->)
%% +=======================+
%%
%% +---+---+---+---+---+---+---+---+
%% |     CRC32     |     ISIZE     |
%% +---+---+---+---+---+---+---+---+
gzip(Uncompressed) ->
    Compressed = gzip_no_header(Uncompressed),
    Header = <<31, 139, 8, 0, 0, 0, 0, 0, 0, 0>>,
    Crc = erlang:crc32(Uncompressed),
    Size = byte_size(Uncompressed),
    Trailer = <<Crc:32/little, Size:32/little>>,
    iolist_to_binary([Header, Compressed, Trailer]).

gzip_no_header(Uncompressed) ->
    Zstream = zlib:open(),

    try
        zlib:deflateInit(Zstream, default, deflated, -15, 8, default),
        Compressed = zlib:deflate(Zstream, Uncompressed, finish),
        zlib:deflateEnd(Zstream),
        iolist_to_binary(Compressed)
    after
        zlib:close(Zstream)
    end.


%%====================================================================
%% Helpers
%%====================================================================

binarify(Term) when is_boolean(Term) ->
    Term;
binarify(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
binarify([]) ->
    [];
binarify(Term) when is_list(Term) ->
    case io_lib:printable_list(Term) of
        true ->
            list_to_binary(Term);
        false ->
            [binarify(X) || X <- Term]
    end;
binarify(Term) when is_map(Term) ->
    List = maps:to_list(Term),
    lists:map(fun({K, V}) -> binarify({K, V}) end, List);
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Term) ->
    Term.

diff_keys(Map, RequiredKeys, OptionalKeys) ->
    Keys = maps:keys(Map),
    MissingKeys = RequiredKeys -- Keys,
    UnknownKeys = Keys -- (RequiredKeys ++ OptionalKeys),

    case {MissingKeys, UnknownKeys} of
        {[], []} ->
            ok;

        {_, [_ | _]} ->
            {error, {unknown_keys, UnknownKeys}};

        _ ->
            {error, {missing_keys, MissingKeys}}
    end.

maybe_update_with(Key, Fun, Map) ->
    case maps:is_key(Key, Map) of
        true ->
            Value = maps:get(Key, Map),
            Map#{Key => apply(Fun, [Value])};

        false ->
            Map
    end.

try_into_map(Fun, List) ->
    maps:from_list(lists:map(Fun, List)).

encode_base16(Binary) ->
    <<X:256/big-unsigned-integer>> = Binary,
    String = string:to_upper(lists:flatten(io_lib:format("~64.16.0b", [X]))),
    list_to_binary(String).

%% Based on https://github.com/goj/base16/blob/master/src/base16.erl
%% (C) 2012, Erlang Solutions Ltd.

decode_base16(Base16) ->
    << <<(unhex(H) bsl 4 + unhex(L))>> || <<H,L>> <= Base16 >>.

unhex(D) when $0 =< D andalso D =< $9 ->
    D - $0;
unhex(D) when $a =< D andalso D =< $f ->
    10 + D - $a;
unhex(D) when $A =< D andalso D =< $F ->
    10 + D - $A.
