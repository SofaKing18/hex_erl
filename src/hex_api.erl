-module(hex_api).
-export([search/2]).

search(Repo, String) ->
    get(Repo, "/packages?sort=total_downloads&search=" ++ String).

%% Private

get(Repo, Path) ->
    case hex_repo:api_url(Repo) of
        Url when is_list(Url) ->
            Body = hex_http:get(Url ++ Path, "application/vnd.hex+erlang"),
            binary_to_term(Body);

        nil ->
            {error, not_supported}
    end.
