-module(hex_repo).
-export([put_repo/2, repos/0, names/1, versions/1, package/2, tarball/3]).

-type repo() :: atom().

-spec repos() -> map().
repos() ->
    case application:get_env(hex_erl, repos) of
        {ok, Map} -> Map;
        undefined -> #{hexpm => "https://repo.hex.pm"}
    end.

-spec put_repo(repo(), string()) -> ok.
put_repo(Repo, Url) ->
    Repos = repos(),
    application:set_env(hex_erl, repos, maps:put(Repo, Url, Repos)),
    ok.

-spec names(repo()) -> [map()].
names(Repo) ->
    Url = maps:get(Repo, repos()),
    Body = hex_http:get(Url ++ "/names"),
    hex_registry:decode_names(Body).

-spec versions(repo()) -> [map()].
versions(Repo) ->
    Url = maps:get(Repo, repos()),
    Body = hex_http:get(Url ++ "/versions"),
    hex_registry:decode_versions(Body).

-spec package(repo(), string()) -> map().
package(Repo, Name) ->
    Url = maps:get(Repo, repos()),
    Body = hex_http:get(Url ++ "/packages/" ++ Name),
    hex_registry:decode_package(Body).

-spec tarball(repo(), string(), string()) -> hex_tarball:tarball().
tarball(Repo, Name, Version) ->
    Url = maps:get(Repo, repos()),
    hex_http:get(Url ++ "/tarballs/" ++ Name ++ "-" ++ Version ++ ".tar").
