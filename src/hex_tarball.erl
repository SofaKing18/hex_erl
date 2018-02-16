%% based on:
%%   https://github.com/hexpm/hex/blob/master/lib/hex/tar.ex
%%   https://github.com/tsloughter/rebar3_hex/blob/master/src/rebar3_hex_tar.erl
-module(hex_tarball).
-export([create/2, unpack/1, format_checksum/1]).
-define(VERSION, <<"3">>).

-type checksum() :: <<_:256>>.
-type metadata() :: map().
-type tarball() :: binary().

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
-spec create(metadata(), [string() | {string(), binary() | string()}]) -> {ok, {tarball(), checksum()}}.
create(Metadata, Files) ->
    MetaBinary = encode_metadata(Metadata),
    ContentsBinary = create_tarball(Files, [compressed]),
    Checksum = checksum(MetaBinary, ContentsBinary),
    ChecksumBase16 = encode_base16(Checksum),

    OuterFiles = [
                  {"VERSION", ?VERSION},
                  {"CHECKSUM", ChecksumBase16},
                  {"metadata.config", MetaBinary},
                  {"contents.tar.gz", ContentsBinary}
                 ],

    Tarball = create_tarball(OuterFiles, []),
    {ok, {Tarball, Checksum}}.

%% @doc
%% Unpacks a package tarball
%%
%% Examples:
%%
%% ```
%%     hex_tarball:unpack(Tarball).
%%     %%=> {ok,{#{<<"app">> => <<"foo">>,
%%     %%=>        <<"version">> => <<"1.0.0">>,
%%     %%=>        ...},
%%     %%=>      <<40,32,...>>,
%%     %%=>      [{"src/foo.erl",<<"-module(foo).">>}}}
%% '''
-spec unpack(tarball()) -> {ok, {metadata(), checksum(), [{string(), binary()}]}}.
unpack(Tarball) ->
    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {"VERSION", _Version} = lists:keyfind("VERSION", 1, OuterFiles),
    {"CHECKSUM", Checksum} = lists:keyfind("CHECKSUM", 1, OuterFiles),
    {"metadata.config", MetaBinary} = lists:keyfind("metadata.config", 1, OuterFiles),
    {"contents.tar.gz", Contents} = lists:keyfind("contents.tar.gz", 1, OuterFiles),
    Metadata = decode_metadata(MetaBinary),
    {ok, Files} = hex_erl_tar:extract({binary, Contents}, [memory, compressed]),
    {ok, {Metadata, decode_base16(Checksum), Files}}.

%% @doc
%% Returns human-readable base16-encoded representation of checksum.
-spec format_checksum(checksum()) -> string().
format_checksum(Checksum) ->
    encode_base16(Checksum).

%% Private

create_tarball(Files, Options) ->
    Path = "tmp.tar",
    ok = hex_erl_tar:create(Path, Files, Options),
    {ok, Tarball} = file:read_file(Path),
    ok = file:delete(Path),
    Tarball.

checksum(MetaString, Contents) ->
    Blob = <<(?VERSION)/binary, MetaString/binary, Contents/binary>>,
    crypto:hash(sha256, Blob).

decode_metadata(Binary) when is_binary(Binary) ->
    String = binary_to_list(Binary),
    case safe_erl_term:string(String) of
        {ok, Tokens, _Line} ->
            Terms = safe_erl_term:terms(Tokens),
            maps:from_list(Terms)
    end.

encode_metadata(Meta) ->
    Data = lists:map(fun(MetaPair) ->
        String = io_lib_pretty:print(binarify(MetaPair), [{encoding, utf8}]),
        unicode:characters_to_binary([String, ".\n"])
      end, maps:to_list(Meta)),
    iolist_to_binary(Data).

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
