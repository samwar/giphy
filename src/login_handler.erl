-module(login_handler).
-behavior(cowboy_rest).

-export([
	init/2,
	allowed_methods/2,
	content_types_provided/2,
  content_types_accepted/2,
	to_html/2,
	login/2
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
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, login}],
		Req, State}.

to_html(Req, State) ->
	{create_login_html(), Req, State}.

login(Req, _State) ->
	{ok, [{<<"username">>, Username}, {<<"password">>, Password}], _Req0} = cowboy_req:read_urlencoded_body(Req),
	Response = case giphy_table_mngr:retrieve_user(Username, Password, true) of
		{ok, User} -> {{true, <<"/search">>}, Req, User};
		{error, _} ->
			{true, cowboy_req:reply(401, #{
				<<"content-type">> => <<"text/html">>
			}, [create_login_html(<<"Wrong username/password.">>)], Req), []}
	end,
	Response.

create_login_html() ->
	create_login_html(<<>>).


create_login_html(ErrorMessage) ->
	<<"<html><body>
<center>
<h1>Welcome to the GIPHY app.</h1>
<h3>Please log in</h3>
<form action=\"/\" method=\"post\">
	<label for=\"username\"><b>Username</b></label>
  <input type=\"text\" name=\"username\" placeholder=\"Enter Username\" required><br>
  <label for=\"password\"><b>Password</b></label>
  <input type=\"password\" name=\"password\" placeholder=\"Enter Password\" required><br>
  <button type=\"submit\">Login</button>
</form>
<font color=\"red\">", ErrorMessage/binary,"</font><br>
<a href=\"/user\">Create an account</a>
</center>
</body></html>">>.
