-module(gif_handler).
-behavior(cowboy_rest).
-include_lib("include/model/giphy_table_definitions.hrl").

-export([
	init/2,
	allowed_methods/2,
	content_types_provided/2,
	content_types_accepted/2,
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

to_html(Req, State) ->
	UserUUID = <<"b3211ff3-1834-4bdf-be5f-b91efd9c81e9">>,
	{ok, Gifs} = giphy_table_mngr:retrieve_gifs(UserUUID),
	% Reverse the list of gifs so they are displayed in the order the user saved them
	HTML = build_html(lists:reverse(Gifs), [], 0),
	{<<"<html><body>", HTML/binary, "</body></html>">>, Req, State}.

from_json(Request, _State) ->
	{ok, Body, Response} = giphy_helper:body(Request, <<>>),
	UserUUID = maps:get(<<"user_uuid">>, Body),
	[begin
		GiphyURI = maps:get(<<"url">>, Gif),
		Categories = maps:get(<<"categories">>, Gif),
		giphy_table_mngr:insert_gif(GiphyURI, Categories, UserUUID)
		end || Gif <- maps:get(<<"gifs">>, Body)],
	{true, Response, _State}.

build_html([], HTML, _Counter) ->
	iolist_to_binary(HTML);
build_html([#gif{giphy_uri = URI, categories = Categories} | Rest], HTML, Counter) ->
	% Convert the categories from a list of binary strings into a comma separated string
	CategoriesString = categories_string(Categories),
	StringyCounter = integer_to_list(Counter),
	NewHTML =
		[
			"<img src=\"", URI, "\" border=\"0\"><br>
			<label for=\"categories_", StringyCounter, "\">Categories:</label>
			<input type=\"text\" name=\"categories_", StringyCounter, "\" id=\"categories_", StringyCounter, "\" value=\"", CategoriesString ,"\"><br>"
		],
	build_html(Rest, lists:append(NewHTML, HTML), Counter + 1).

categories_string([]) ->
	"";
categories_string(Categories) ->
	[StringyCategories] = io_lib:format("~s", [Categories]),
	string:join(StringyCategories, ", ").

