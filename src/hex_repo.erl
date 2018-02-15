-module(hex_repo).
-export([api_url/1, repo_url/1, names/1, versions/1, package/2, tarball/3]).

-type repo() :: hexpm | #{ api_url := string() | nil, repo_url := string() }.

-spec api_url(repo()) -> string() | nil.
api_url(Repo) ->
    maps:get(api_url, repo(Repo)).

-spec repo_url(repo()) -> string().
repo_url(Repo) ->
    maps:get(repo_url, repo(Repo)).

-spec names(repo()) -> {ok, [#{name => binary()}]} | {error, term()}.
names(Repo) ->
    case get(Repo, "/names") of
        {ok, Body} ->
            {ok, hex_registry:decode_names(Body)};

        Other ->
            Other
    end.

-spec versions(repo()) ->
        {ok, [#{name => binary(),
                retired => [hex_tarball:version()],
                versions => [hex_tarball:version()]}]} | {error, term()}.
versions(Repo) ->
    case get(Repo, "/versions") of
        {ok, Body} ->
            {ok, hex_registry:decode_versions(Body)};

        Other ->
            Other
    end.

-spec package(repo(), string()) ->
        {ok, #{releases => [#{checksum => hex_tarball:checksum(),
                         dependencies => [map()],
                         version => hex_tarball:version()}]}} | {error, term()}.
package(Repo, Name) ->
    case get(Repo, "/packages/" ++ Name) of
        {ok, Body} ->
            {ok, hex_registry:decode_package(Body)};

        Other ->
            Other
    end.

-spec tarball(repo(), string(), string()) -> {ok, hex_tarball:tarball()} | {error, term()}.
tarball(Repo, Name, Version) ->
    get(Repo,  "/tarballs/" ++ Name ++ "-" ++ Version ++ ".tar").

%% Private

get(Repo, Path) ->
    Url = repo_url(Repo),
    case hex_http:get(Url ++ Path, "application/octet-stream") of
        {ok, {200, _, Body}} ->
            {ok, Body};

        Other ->
            Other
    end.

repo(hexpm) ->
    #{
       api_url => "https://hex.pm/api",
       repo_url => "https://repo.hex.pm"
     };
repo(Repo) when is_map(Repo) ->
    Repo.
