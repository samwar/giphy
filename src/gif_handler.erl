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
      RespBody = cowboy_req:set_resp_body(<<"User not found">>, Req),
      {false, RespBody, no_state};
    {ok, User} ->
      {true, Req, User}
  end.

to_html(Req, #user{uuid = UserUUID} = User) ->
  {ok, Gifs} = giphy_table_mngr:retrieve_gifs_by_user_id(UserUUID),
  UserHTML = build_user_html(User),
  % Reverse the list of gifs so they are displayed in the order the user saved them
  GifHTML = build_gif_html(lists:reverse(Gifs), [], UserUUID, length(Gifs)),
  {<<"<html><body>\n", UserHTML/binary, "\n", GifHTML/binary, "\n</body></html>">>, Req, User}.

from_json(Request, _State) ->
  {ok, #{<<"user_uuid">> := UserUUID, <<"gifs">> := Gifs}, Response} = giphy_helper:body(Request, <<>>),
  [giphy_table_mngr:insert_gif(GiphyURI, Categories, UserUUID)
    || #{<<"url">> := GiphyURI, <<"categories">> := Categories} <- Gifs],
  {true, Response, _State}.

build_user_html(#user{username = Username, uuid = UserUUID}) ->
  <<"<h1>", Username/binary, "'s favorite gifs!</h1>\n",
    "<h3><a href=\"/search/", UserUUID, "\">Search for gifs!</a></h3>">>.

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
      "<img id=\"", GifId, "\" src=\"", URI, "\" border=\"0\"><br>\n",
      "<label for=\"", GifId, "\">Categories:</label>\n",
      "<input type=\"text\" name=\"", GifId, "\" id=\"", GifId, "\" value=\"", CategoriesString, "\">\n"
    "<input type=\"checkbox\" name=\"", GifId, "\">Delete<br>\n"
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

update_save_or_delete_gifs([{<<"_method">>, <<"put">>} | FormData], _UserId) ->
  [update_or_delete_gif(Data) || Data <- FormData];
update_save_or_delete_gifs(FormData, UserId) ->
  [giphy_table_mngr:insert_gif(GifId, GifUrl, [], UserId) || {GifId, GifUrl} <- FormData].

update_or_delete_gif({GifId, <<"on">>}) ->
  giphy_table_mngr:delete_gif(GifId);
update_or_delete_gif({GifId, Categories}) ->
  CategoryList = binary:split(Categories, [<<",">>, <<", ">>], [global]),
  giphy_table_mngr:update_gif(GifId, CategoryList).
