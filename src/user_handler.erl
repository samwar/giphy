-module(user_handler).
-behavior(cowboy_rest).
-include_lib("include/model/giphy_table_definitions.hrl").

-export([
  init/2,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  to_html/2,
  create_user_from_form/2
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
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, create_user_from_form}],
    Req, State}.

to_html(Req, State) ->
  {create_user_html(), Req, State}.

create_user_from_form(Req, _State) ->
  {ok, FormData, _Req0} = cowboy_req:read_urlencoded_body(Req),
  [{<<"username">>, Username}, {<<"password">>, Password}, {<<"email">>, Email}] = FormData,
  {ok, ok} = giphy_table_mngr:insert_user(Username, Password, Email),
  {ok, #user{uuid = UserUUID} = User} = giphy_table_mngr:retrieve_user(Username, Password, true),
  {{true, <<"gifs/", UserUUID/binary>>}, Req, User}.

create_user_html() ->
  Style = giphy_helper:build_login_user_style(),
  <<"<html><body>",
    Style/binary,"
<form action=\"/user\" method=\"post\">
<div class=\"container\">
<h3>Please create an account</h3>
	<label for=\"username\"><b>Username</b></label>
  <input type=\"text\" name=\"username\" placeholder=\"Enter Username\" required><br>
  <label for=\"password\"><b>Password</b></label>
  <input type=\"password\" name=\"password\" placeholder=\"Enter Password\" required><br>
  <label for=\"email\"><b>Email</b></label>
  <input type=\"text\" name=\"email\" placeholder=\"Enter Email Address\" required><br>
  <button type=\"submit\">Create Account</button>
</form>
</div>
</body></html>">>.
