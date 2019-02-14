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
  {create_user_html(<<>>), Req, State}.

create_user_from_form(Req, _State) ->
  {ok, FormData, _Req0} = cowboy_req:read_urlencoded_body(Req),
  [{<<"username">>, Username}, {<<"password">>, Password}, {<<"email">>, Email}] = FormData,
  CreateUserResult = create_user(Username, Password, Email),
  navigate_to_profile_or_return_error(CreateUserResult, Req).

create_user_html(ErrorMessage) ->
  Style = giphy_request_helper:build_login_user_style(),
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
  <p>", ErrorMessage/binary, "</p>
  <a href=\"/\">Already have an account?</a>
</form>
</div>
</body></html>">>.

%% Checks to see if a user with the username already exists. If it does, return an error
%% or else create the user and move onto the search page to start adding gifs
create_user(Username, Password, Email) ->
  case giphy_table_mngr:retrieve_user(Username, undefined, false) of
    {ok, #user{}} ->
      {error, {username_exists, <<"This username already exists">>}};
    {error, not_found} ->
      {ok, ok} = giphy_table_mngr:insert_user(Username, Password, Email),
      giphy_table_mngr:retrieve_user(Username, Password, true)
  end.

navigate_to_profile_or_return_error({error, {username_exists, ErrorMessage}}, Req) ->
  {true, cowboy_req:reply(401, #{
    <<"content-type">> => <<"text/html">>
  }, [create_user_html(ErrorMessage)], Req), []};
navigate_to_profile_or_return_error({ok, #user{uuid = UserUUID} = User}, Req) ->
  {{true, <<"search/", UserUUID/binary>>}, Req, User}.
