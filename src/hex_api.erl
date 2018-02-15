-module(hex_api).
-export([search/2]).

search(Repo, String) ->
    get(Repo, "/packages?sort=total_downloads&search=" ++ String).

%% Private

get(Repo, Path) ->
    case hex_repo:api_url(Repo) of
        Url when is_list(Url) ->
            case hex_http:get(Url ++ Path, "application/vnd.hex+erlang") of
                {ok, {200, _, Body}} ->
                    binary_to_term(Body);

                Other ->
                    Other
            end;

        nil ->
            {error, not_supported}
    end.
