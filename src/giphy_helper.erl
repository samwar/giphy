%%%-------------------------------------------------------------------
%%% @author Sam Warters
%%% @doc
%%% Some helper functions for the giphy app including a way to quickly
%%% seed some test data.
%%% @end
%%% Created : 27. Jan 2019 12:37
%%%-------------------------------------------------------------------
-module(giphy_helper).

-include_lib("include/model/giphy_table_definitions.hrl").

%% API
-export([
  uuid_v4/0,
  body/2,
  seed_tables_for_testing/0,
  build_search_and_filter_style/0,
  build_login_user_style/0
]).

%% Creates a version 4 UUID string, which is randomly generated and looks
%% like "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx", where the x's are hex digits,
%% the 4 is constant, and y is special because its first two bits are 10 to
%% indicate the "variant".
%% See http://en.wikipedia.org/wiki/Universally_unique_identifier
-spec uuid_v4() -> string().
uuid_v4() ->
  % Remember: bin matching counts by bits by default...
  <<A:32, B:16, _:4, C:12, _:2, D:14, E:48>> = crypto:strong_rand_bytes(16),
  CWithVersion = C bor 16#4000, % Insert version == 4
  DWithVariant = D bor 16#8000, % Insert variant == 1,0 as first two bits
  FormatString = "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
  lists:flatten(io_lib:format(FormatString, [A, B, CWithVersion, DWithVariant, E])).

%% Helper to peel the entire body out of a json payload and return it in a jiffy decoded map
body(Request, Acc) ->
  {ok, Body, Response} = case cowboy_req:read_body(Request) of
    {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
    {more, Data, Req} -> body(Req, <<Acc/binary, Data/binary>>)
  end,
  DecodedBody = jiffy:decode(Body, [return_maps]),
  {ok, DecodedBody, Response}.

%% A quick way to seed the mnesia tables for testing. Returns the UserUUID you'll need to retrieve and save gifs
seed_tables_for_testing() ->
  {ok, ok} = giphy_table_mngr:insert_user(<<"test_uuid">>, <<"Samwar">>, <<"test">>, <<"samwar@gmail.com">>),
  {ok, User} = giphy_table_mngr:retrieve_user(<<"Samwar">>, <<"test">>, true),
  UserUUID = User#user.uuid,
  giphy_table_mngr:insert_gif(iolist_to_binary([UserUUID, "_FiGiRei2ICzzG"]), <<"https://media2.giphy.com/media/FiGiRei2ICzzG/giphy.gif">>, [<<"funny">>, <<"cat">>, <<"roomba">>], UserUUID),
  giphy_table_mngr:insert_gif(iolist_to_binary([UserUUID, "_feqkVgjJpYtjy"]), <<"https://media0.giphy.com/media/feqkVgjJpYtjy/giphy.gif">>, [<<"bird">>, <<"zoom">>], UserUUID),
  UserUUID.

%% All of this CSS is stolen right from the W3 Schools. Thanks to them for helping a non-web designer out.
build_search_and_filter_style() -> <<"
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

form.filter input[type=text] {
  padding: 10px;
  font-size: 17px;
  border: 1px solid grey;
  float: left;
  width: 60%;
  background: #f1f1f1;
}

form.filter button {
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

form.filter button:hover {
  background: #0b7dda;
}

form.filter::after {
  content: "";
  clear: both;
  display: table;
}

button {
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
</style>">>.

build_login_user_style() -> <<"
<style>
body {
  font-family: Arial;
}

h1 {
 text-align: center;
}

h3 {
 text-align: center;
}

a {
 text-align: center;
}

p {
  color: red;
  text-align: center;
}

/* Bordered form */
form {
  border: 3px solid #f1f1f1;
  margin: auto;
  width: 50%
}

/* Full-width inputs */
input[type=text], input[type=password] {
  width: 100%;
  padding: 12px 20px;
  margin: 8px 0;
  display: inline-block;
  border: 1px solid #ccc;
  box-sizing: border-box;
}

/* Set a style for all buttons */
button {
  background-color: #2196F3;
  color: white;
  padding: 14px 20px;
  margin: 8px 0;
  border: none;
  cursor: pointer;
  width: 100%;
}

/* Add a hover effect for buttons */
button:hover {
  opacity: 0.8;
}

/* Add padding to containers */
.container {
  padding: 16px;
}
</style>">>.
