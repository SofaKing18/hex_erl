-module(hex_api).
-export([search/1]).

search(String) ->
    hex_http:get("https://hex.pm/api/packages?sort=total_downloads&search=" ++ String).
