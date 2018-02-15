-module(hex_repo).
-export([api_url/1, repo_url/1, names/1, versions/1, package/2, tarball/3]).

-type repo() :: atom() | #{ api_url => string() | nil, repo_url => string() }.

-spec api_url(repo()) -> string().
api_url(Repo) ->
    maps:get(api_url, repo(Repo)).

-spec repo_url(repo()) -> string().
repo_url(Repo) ->
    maps:get(repo_url, repo(Repo)).

-spec names(repo()) -> [map()].
names(Repo) ->
    Body = get(Repo, "/names"),
    hex_registry:decode_names(Body).

-spec versions(repo()) -> [map()].
versions(Repo) ->
    Body = get(Repo, "/versions"),
    hex_registry:decode_versions(Body).

-spec package(repo(), string()) -> map().
package(Repo, Name) ->
    Body = get(Repo, "/packages/" ++ Name),
    hex_registry:decode_package(Body).

-spec tarball(repo(), string(), string()) -> hex_tarball:tarball().
tarball(Repo, Name, Version) ->
    get(Repo,  "/tarballs/" ++ Name ++ "-" ++ Version ++ ".tar").

%% Private

get(Repo, Path) ->
    Url = repo_url(Repo),
    hex_http:get(Url ++ Path, "application/octet-stream").

repo(hexpm) ->
    #{
       api_url => "https://hex.pm/api",
       repo_url => "https://repo.hex.pm"
     };
repo(Repo) when is_map(Repo) ->
    Repo.