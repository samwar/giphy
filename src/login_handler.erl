-module(login_handler).
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
	{
		<<"<html><body>
<h1>Welcome to the GIPHY app.</h1>
<h3>Please log in</h3>
<form action=\"/login\" method=\"post\">
  Username: <input type=\"text\" name=\"username\"><br>
  Password: <input type=\"password\" name=\"password\"><br>
  <input type=\"submit\" value=\"Login\">
</form>
</body></html>">>,
		Req, State}.
