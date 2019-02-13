%%%-------------------------------------------------------------------
%%% @author Sam Warters
%%% @doc
%%% A module containing functions for use in the http request process
%%% @end
%%% Created : 27. Jan 2019 12:37
%%%-------------------------------------------------------------------
-module(giphy_request_helper).
-include_lib("include/model/giphy_table_definitions.hrl").

%% API
-export([
  body/2,
  build_search_and_filter_style/0,
  build_login_user_style/0
]).

%% Helper to peel the entire body out of a json payload and return it in a jiffy decoded map
body(Request, Acc) ->
  {ok, Body, Response} = case cowboy_req:read_body(Request) of
    {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
    {more, Data, Req} -> body(Req, <<Acc/binary, Data/binary>>)
  end,
  DecodedBody = jiffy:decode(Body, [return_maps]),
  {ok, DecodedBody, Response}.

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

.container {
  padding: 10px;
  border: 1px solid grey;
}

input[type=text] {
  padding: 5px;
  font-size: 14px;
  border: 1px solid grey;
  width: 25%;
  background: #f1f1f1;
}

img {
  border: 1px solid grey;
  width: 34%;
}

/* Add a black background color to the top navigation */
.topnav {
  background-color: #333;
  overflow: hidden;
}

/* Style the links inside the navigation bar */
.topnav a {
  float: left;
  color: #f2f2f2;
  text-align: center;
  padding: 14px 16px;
  text-decoration: none;
  font-size: 17px;
}

/* Change the color of links on hover */
.topnav a:hover {
  background-color: #ddd;
  color: black;
}

/* Add a color to the active/current link */
.topnav a.active {
  background-color: #2196F3;
  color: white;
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
