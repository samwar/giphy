-module(search_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([content_types_provided/2]).
-export([to_html/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, '*'}, to_html}
	], Req, State}.

to_html(Req, State) ->
	Style = build_style(),
	#{search := SearchString} = cowboy_req:match_qs([{search, [], <<>>}], Req),
	SearchForm = build_search_form(SearchString),
	FormattedSearchString = binary:replace(SearchString, <<" ">>, <<"+">>, [global]),
	SearchResults = do_giphy_search(FormattedSearchString),
	% Reverse the map of images so after the recurssion they show up in the order that giphy served them up
	Data = lists:reverse(maps:get(<<"data">>, SearchResults)),
	SearchHTML = build_search_results(Data, [], <<"test_uuid">>),
	{<<"<html><body>", Style/binary, SearchForm/binary, SearchHTML/binary,"</body></html>">>, Req, State}.

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
<input type=\"text\" value=\"", SearchString/binary,"\" placeholder=\"Search..\" name=\"search\">
<button type=\"submit\">Search</button>
</form><br><br>">>.

do_giphy_search(<<>>) -> #{<<"data">> => []};
do_giphy_search(FormattedSearchString) ->
	GiphyAPIKey = giphy_api_key(),
	URL = binary_to_list(iolist_to_binary(["http://api.giphy.com/v1/gifs/search?q=", FormattedSearchString, "&api_key=",GiphyAPIKey,"&rating=g&limit=5"])),
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(URL),
	jiffy:decode(Body, [return_maps]).

build_search_results([], HTML, _UserId) ->
	iolist_to_binary(HTML);
build_search_results([#{<<"id">> := GifId, <<"images">> := #{<<"original">> := #{<<"url">> := Url}}}| Rest], HTML, UserId) ->
	UniqueGifId = <<UserId/binary, "_", GifId/binary>>,
	ImageHTML = [
		"<img id=\"", UniqueGifId, "\"src=\"", Url, "\" border=\"0\">",
		"<input type=\"checkbox\" name=\"", UniqueGifId, "\" value=\"",Url, "\"><br>"
	],
	build_search_results(Rest, lists:append(ImageHTML, HTML), UserId).

giphy_api_key() -> <<"u5Chd72cwVYT20iP2nj308IhEyESERut">>.
