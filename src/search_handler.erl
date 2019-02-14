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
      HTML = <<"<html><body>User not found.\n<br><a href=\"/user\">Please make an account.</a></html></body>">>,
      RespBody = cowboy_req:set_resp_body(HTML, Req),
      {false, RespBody, no_state};
    {ok, User} ->
      {true, Req, User}
  end.

to_html(Req, #user{uuid = UserId} = State) ->
  Style = giphy_request_helper:build_search_and_filter_style(),
  NavBarHTML = build_navbar_html(State),
  #{search := SearchString} = cowboy_req:match_qs([{search, [], no_search_string}], Req),
  SearchForm = build_search_form(SearchString, UserId),
  SearchResults = do_giphy_search(SearchString),
  % Reverse the map of images so after the recurssion they show up in the order that giphy served them up
  Data = lists:reverse(maps:get(<<"data">>, SearchResults)),
  SearchHTML = build_search_results(Data, [], UserId),
  {<<"<html>\n<body>\n", NavBarHTML/binary, Style/binary, SearchForm/binary, SearchHTML/binary, "\n</body>\n</html>">>,
    Req, State}.

build_navbar_html(#user{username = Username, uuid = UserUUID}) ->
  iolist_to_binary([
    "<div class=\"topnav\">\n",
    "\t<a href=\"/gifs/", UserUUID, "\">", string:titlecase(Username), "'s Favorite Gifs</a>\n",
    "\t<a class=\"active\" href=\"#search_giphy\">Search GIPHY</a>\n",
    "\t<a href=\"/\">Log Out</a>\n",
    "</div>"
  ]).

build_search_form(no_search_string, UserUUID) -> do_build_search_form(<<>>, UserUUID);
build_search_form(SearchString, UserUUID) -> do_build_search_form(SearchString, UserUUID).

do_build_search_form(SearchString, UserUUID) -> <<"
<div>
  <form class=\"search\" action=\"/search/", UserUUID/binary,"\">
    <input type=\"text\" value=\"", SearchString/binary, "\" placeholder=\"Search Giphy..\" name=\"search\">
    <button type=\"submit\">Search</button>
  </form>
</div>">>.

do_giphy_search(no_search_string) -> #{<<"data">> => [no_search_results]};
do_giphy_search(SearchString) ->
  FormattedSearchString = binary:replace(SearchString, <<" ">>, <<"+">>, [global]),
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
    "<div class=\"container\">\n"
    "<img id=\"", UniqueGifId, "\"src=\"", Url, "\"><br>\n",
    "<input type=\"checkbox\" name=\"", UniqueGifId, "\" value=\"", Url, "\">Add to favorites\n"
    "</div>\n"
  ],
  build_search_results(Rest, lists:append(ImageHTML, HTML), UserId).

giphy_api_key() -> <<"u5Chd72cwVYT20iP2nj308IhEyESERut">>.
