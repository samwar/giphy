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
	from_json/2

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
		{{<<"application">>, <<"json">>, '*'}, from_json}
	], Req, State}.

resource_exists(Req, _State) ->
	UserUUID = cowboy_req:binding(user_uuid, Req),
	case giphy_table_mngr:retrieve_user_by_uuid(UserUUID) of
		{error, not_found} ->
			lager:error("User id not found: ~p", [UserUUID]),
			RespBody = cowboy_req:set_resp_body(<<"User not found">>, Req),
			{false, RespBody, no_state};
		{ok, User} ->
			{true, Req, User}
	end.

to_html(Req, #user{uuid = UserUUID} = User) ->
%%	UserUUID = <<"b3211ff3-1834-4bdf-be5f-b91efd9c81e9">>,
	{ok, Gifs} = giphy_table_mngr:retrieve_gifs(UserUUID),
	UserHTML = build_user_html(User),
	% Reverse the list of gifs so they are displayed in the order the user saved them
	GifHTML = build_gif_html(lists:reverse(Gifs), []),
	{<<"<html><body>\n", UserHTML/binary, "\n", GifHTML/binary, "\n</body></html>">>, Req, User}.

from_json(Request, _State) ->
	{ok, Body, Response} = giphy_helper:body(Request, <<>>),
	UserUUID = maps:get(<<"user_uuid">>, Body),
	[begin
		GiphyURI = maps:get(<<"url">>, Gif),
		Categories = maps:get(<<"categories">>, Gif),
		giphy_table_mngr:insert_gif(GiphyURI, Categories, UserUUID)
		end || Gif <- maps:get(<<"gifs">>, Body)],
	{true, Response, _State}.

build_user_html(#user{username = Username}) ->
	<<"<h1>",Username/binary, "'s favorite gifs!</h1>">>.

build_gif_html([], HTML) ->
	iolist_to_binary(HTML);
build_gif_html([#gif{gif_id = GifId, giphy_uri = URI, categories = Categories} | Rest], HTML) ->
	% Convert the categories from a list of binary strings into a comma separated string
	CategoriesString = categories_string(Categories),
	NewHTML =
		[
			"<img id=\"", GifId, "\" src=\"", URI, "\" border=\"0\"><br>
			<label for=\"categories_", GifId, "\">Categories:</label>
			<input type=\"text\" name=\"categories_", GifId, "\" id=\"categories_", GifId, "\" value=\"", CategoriesString ,"\"><br>"
		],
	build_gif_html(Rest, lists:append(NewHTML, HTML)).

categories_string([]) ->
	"";
categories_string(Categories) ->
	[StringyCategories] = io_lib:format("~s", [Categories]),
	string:join(StringyCategories, ", ").

