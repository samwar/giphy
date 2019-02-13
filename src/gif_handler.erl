-module(gif_handler).
-behavior(cowboy_rest).
-include_lib("include/model/giphy_table_definitions.hrl").

-export([
  init/2,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  resource_exists/2,
  to_html/2,
  from_json/2,
  from_form/2
]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"text">>, <<"html">>, '*'}, to_html}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, '*'}, from_json},
    {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, from_form}
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

to_html(Req, #user{uuid = UserUUID} = User) ->
  #{filter := FilterString} = cowboy_req:match_qs([{filter, [], no_filter}], Req),
  Style = giphy_helper:build_search_and_filter_style(),
  NavBarHTML = build_navbar_html(User),
  SearchForm = build_search_form(FilterString, UserUUID),
  {ok, Gifs} = find_gifs(FilterString, UserUUID),
  % Reverse the list of gifs so they are displayed in the order the user saved them
  GifHTML = build_gif_html(lists:reverse(Gifs), [], UserUUID, length(Gifs)),
  {<<"<html><body>\n", Style/binary, "\n", NavBarHTML/binary, SearchForm/binary, GifHTML/binary, "\n</body></html>">>,
    Req, User}.

from_json(Request, _State) ->
  {ok, #{<<"user_uuid">> := UserUUID, <<"gifs">> := Gifs}, Response} = giphy_helper:body(Request, <<>>),
  [giphy_table_mngr:insert_gif(GiphyURI, Categories, UserUUID)
    || #{<<"url">> := GiphyURI, <<"categories">> := Categories} <- Gifs],
  {true, Response, _State}.

build_navbar_html(#user{username = Username, uuid = UserUUID}) ->
  iolist_to_binary([
    "<div class=\"topnav\">\n",
    "\t<a class=\"active\" href=\"#favorite_gifs\">", string:titlecase(Username), "'s Favorite Gifs</a>\n",
    "\t<a href=\"/search/", UserUUID, "\">Search GIPHY</a>\n",
    "\t<a href=\"/\">Log Out</a>\n",
    "</div>"
  ]).

build_search_form(no_filter, UserUUID) ->
  do_build_search_form(<<>>, UserUUID);
build_search_form(SearchString, UserUUID) ->
  do_build_search_form(SearchString, UserUUID).

do_build_search_form(SearchString, UserUUID) -> <<"
<div>
  <form class=\"filter\" action=\"/gifs/", UserUUID/binary,"\">
    <input type=\"text\" value=\"", SearchString/binary, "\" placeholder=\"Filter favorite gifs by category..\" name=\"filter\">
    <button type=\"submit\">Filter</button>
  </form>
  <form class=\"filter\" action=\"/gifs/", UserUUID/binary,"\">
    <button type=\"submit\">Clear Filter</button>
  </form>
</div>"
>>.

%% Builds the html to display the gifs. If there are non retrieved from the database, don't build anything.
build_gif_html([], _HTML, _UserId, 0) -> <<>>;
build_gif_html([], HTML, UserId, _InitialGifCount) ->
  iolist_to_binary([
    "<form action=\"/gifs/", UserId, "\" method=\"post\">\n",
    "<input type=\"hidden\" name=\"_method\" value=\"put\" />\n",
    HTML,
    "<button type=\"submit\">Save</button>\n</form>"]);
build_gif_html([#gif{gif_id = GifId, giphy_uri = URI, categories = Categories} | Rest], HTML, UserId, InitialGifCount) ->
  % Convert the categories from a list of binary strings into a comma separated string
  CategoriesString = categories_string(Categories),
  NewHTML =
    [
      "<div class=\"container\">\n"
      "<img id=\"", GifId, "\" src=\"", URI, "\"><br>\n",
      "<label for=\"", GifId, "\">Categories:</label>\n",
      "<input type=\"text\" name=\"", GifId, "\" id=\"", GifId, "\" value=\"", CategoriesString, "\"placeholder=\"ex: raining, cats, dogs, small animals\">\n"
      "<input type=\"checkbox\" name=\"", GifId, "\">Delete<br>\n"
      "</div>\n"
    ],
  build_gif_html(Rest, lists:append(NewHTML, HTML), UserId, InitialGifCount).

% Convert the categories from a list of binary strings into a comma separated string
categories_string([]) -> "";
categories_string(Categories) ->
  [StringyCategories] = io_lib:format("~s", [Categories]),
  string:join(StringyCategories, ", ").

from_form(Request, #user{uuid = UserId} = State) ->
  {ok, FormData, _Req} = cowboy_req:read_urlencoded_body(Request),
  update_save_or_delete_gifs(FormData, UserId),
  {{true, <<"/gifs/", UserId/binary>>}, Request, State}.

find_gifs(no_filter, UserUUID) ->
  giphy_table_mngr:retrieve_gifs_by_user_id(UserUUID);
find_gifs(FilterString, UserUUID) ->
  Categories = binary:split(FilterString, <<" ">>),
  giphy_table_mngr:retrieve_gifs_by_categories(UserUUID, Categories).

update_save_or_delete_gifs([{<<"_method">>, <<"put">>} | FormData], _UserId) ->
  [update_or_delete_gif(Data) || Data <- FormData];
update_save_or_delete_gifs(FormData, UserId) ->
  [giphy_table_mngr:insert_gif(GifId, GifUrl, [], UserId) || {GifId, GifUrl} <- FormData].

update_or_delete_gif({GifId, <<"on">>}) ->
  giphy_table_mngr:delete_gif(GifId);
update_or_delete_gif({GifId, Categories}) ->
  CategoryList = binary:split(Categories, [<<",">>, <<", ">>], [global]),
  giphy_table_mngr:update_gif(GifId, CategoryList).
