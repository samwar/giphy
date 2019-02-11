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
  seed_tables_for_testing/0
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
    {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
    {more, Data, Req} -> body(Req, << Acc/binary, Data/binary >>)
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
