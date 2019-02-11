-module(search_handler).
-behavior(cowboy_rest).
-include_lib("include/model/giphy_table_definitions.hrl").

-export([
  init/2,
  content_types_provided/2,
  resource_exists/2,
  to_html/2
]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"text">>, <<"html">>, '*'}, to_html}
  ], Req, State}.

resource_exists(Req, _State) ->
  UserUUID = cowboy_req:binding(user_uuid, Req),
  case giphy_table_mngr:retrieve_user_by_uuid(UserUUID) of
    {error, not_found} ->
      RespBody = cowboy_req:set_resp_body(<<"User not found">>, Req),
      {false, RespBody, no_state};
    {ok, User} ->
      {true, Req, User}
  end.

to_html(Req, #user{uuid = UserId} = State) ->
  Style = build_style(),
  #{search := SearchString} = cowboy_req:match_qs([{search, [], <<>>}], Req),
  SearchForm = build_search_form(SearchString),
  FormattedSearchString = binary:replace(SearchString, <<" ">>, <<"+">>, [global]),
  SearchResults = do_giphy_search(FormattedSearchString),
  % Reverse the map of images so after the recurssion they show up in the order that giphy served them up
  Data = lists:reverse(maps:get(<<"data">>, SearchResults)),
  SearchHTML = build_search_results(Data, [], UserId),
  {<<"<html><body>", Style/binary, SearchForm/binary, SearchHTML/binary, "\n</body></html>">>, Req, State}.

build_style() -> <<"
<style>
body {
  font-family: Arial;
}

* {
  box-sizing: border-box;
}

form.search input[type=text] {
  padding: 10px;
  font-size: 17px;
  border: 1px solid grey;
  float: left;
  width: 80%;
  background: #f1f1f1;
}

form.search button {
  float: left;
  width: 20%;
  padding: 10px;
  background: #2196F3;
  color: white;
  font-size: 17px;
  border: 1px solid grey;
  border-left: none;
  cursor: pointer;
}

form.search button:hover {
  background: #0b7dda;
}

form.search::after {
  content: "";
  clear: both;
  display: table;
}
</style>">>.

build_search_form(<<>>) -> do_build_search_form(<<>>);
build_search_form(SearchString) -> do_build_search_form(SearchString).

do_build_search_form(SearchString) -> <<"
<form class=\"search\" action=\"/search\">
<input type=\"text\" value=\"", SearchString/binary, "\" placeholder=\"Search..\" name=\"search\">
<button type=\"submit\">Search</button>
</form><br><br>\n">>.

do_giphy_search(<<>>) -> #{<<"data">> => [no_search_results]};
do_giphy_search(FormattedSearchString) ->
  GiphyAPIKey = giphy_api_key(),
  URL = binary_to_list(iolist_to_binary(["http://api.giphy.com/v1/gifs/search?q=", FormattedSearchString, "&api_key=", GiphyAPIKey, "&rating=g&limit=6"])),
  %% If giphy is down, this won't return a 200. It would be best to build some error handling here.
  {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(URL),
  jiffy:decode(Body, [return_maps]).

build_search_results([no_search_results], _HTML, _UserId) -> <<>>;
build_search_results([], HTML, UserId) ->
  iolist_to_binary([
    "<form class=\"search\" action=\"/gifs/", UserId, "\" method=\"post\">\n",
    HTML,
    "<button type=\"submit\">Save</button>\n</form>"]);
build_search_results([#{<<"id">> := GifId, <<"images">> := #{<<"original">> := #{<<"url">> := Url}}} | Rest], HTML, UserId) ->
  UniqueGifId = <<UserId/binary, "_", GifId/binary>>,
  ImageHTML = [
    "<img id=\"", UniqueGifId, "\"src=\"", Url, "\" border=\"0\">\n",
    "<input type=\"checkbox\" name=\"", UniqueGifId, "\" value=\"", Url, "\"><br>\n"
  ],
  build_search_results(Rest, lists:append(ImageHTML, HTML), UserId).

giphy_api_key() -> <<"u5Chd72cwVYT20iP2nj308IhEyESERut">>.
