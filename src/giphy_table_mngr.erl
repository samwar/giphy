%%%-------------------------------------------------------------------
%%% @author Sam Warters
%%% @doc
%%% A module for managing the mnesia tables. This includes functions
%%% for ensuring table creation, as well as searching and CRUD operations.
%%% @end
%%% Created : 27. Jan 2019 12:01
%%%-------------------------------------------------------------------
-module(giphy_table_mngr).

-include_lib("include/model/giphy_table_definitions.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
  create_tables/0,
  insert_user/3,
  insert_user/4,
  retrieve_user/3,
  retrieve_user_by_uuid/1,
  insert_gif/4,
  update_gif/2,
  delete_gif/1,
  retrieve_gifs_by_user_id/1,
  retrieve_gifs_by_categories/2,
  uuid_v4/0,
  seed_tables_for_testing/0
]).


create_tables() ->
  % Create the user table, build an index on username and uuid
  mnesia:create_table(user, [
    {index, [username]},
    {attributes, record_info(fields, user)}
  ]),

  % Create the gif table, build an index on giphy_uri and user_uuid
  mnesia:create_table(gif, [
    {index, [giphy_uri, user_uuid]},
    {attributes, record_info(fields, gif)}
  ]).

insert_user(Username, Password, Email) ->
  insert_user(undefined, Username, Password, Email).

insert_user(undefined, Username, Password, Email) ->
  insert_user(list_to_binary(uuid_v4()), Username, Password, Email);
insert_user(UUID, Username, Password, Email) ->
  User = #user{
    uuid = UUID,
    username = string:lowercase(Username), % Make all usernames case insensitive
    password = base64:encode(Password), % SUPER DUPER secure base64 encoding. Lolz.
    email = Email
  },
  Fun = fun() ->
    mnesia:write(User)
  end,
  normalize_return(mnesia:transaction(Fun)).

retrieve_user(Username, Password, AuthCheck) ->
  Fun = fun() ->
    Query = qlc:q([User || User <- mnesia:table(user),
      User#user.username == string:lowercase(Username)]), % Search for usernames in a case insensitive manner
    qlc:e(Query)
  end,
  do_retrieve_user(mnesia:transaction(Fun), Password, AuthCheck).

retrieve_user_by_uuid(UUID) ->
  Fun = fun() ->
    mnesia:read({user, UUID})
  end,
  do_retrieve_user(mnesia:transaction(Fun), undefined, false).

do_retrieve_user({atomic, []}, _Password, _AuthCheck) ->
  {error, not_found};
do_retrieve_user({atomic, [User]}, Password, true) ->
  EncodedPassword = base64:encode(Password),
  authorized_user(EncodedPassword, User);
do_retrieve_user({atomic, [User]}, _Password, false) ->
  {ok, User#user{password = undefined}};
do_retrieve_user({abort, _Reason} = Result, _Password, _AuthCheck) ->
  Result.

authorized_user(Password, #user{password = Password} = User) ->
  % Return the user without the password so it isn't flying around in the UI.
  UserNoPassword = User#user{password = undefined},
  {ok, UserNoPassword};
authorized_user(_EncodedPassword, #user{password = _Password}) ->
  {error, {unauthorized_user, <<"The user is unauthorized">>}}.

insert_gif(GifId, GiphyURI, Categories, UserUUID) ->
  Gif = #gif{
    gif_id = GifId,
    giphy_uri = GiphyURI,
    categories = Categories,
    user_uuid = UserUUID
  },
  Fun = fun() ->
    mnesia:write(Gif)
  end,
  normalize_return(mnesia:transaction(Fun)).

update_gif(GifId, Categories) ->
  RetrieveGifFun = fun() ->
    mnesia:read({gif, GifId})
  end,
  case mnesia:transaction(RetrieveGifFun) of
    {atomic, [#gif{giphy_uri = GiphyURI, user_uuid = UserUUID}]} ->
      insert_gif(GifId, GiphyURI, Categories, UserUUID);
    {aborted, Reason} -> {error, Reason}
  end.

delete_gif(GifId) ->
  Fun = fun() ->
    mnesia:delete({gif, GifId})
  end,
  normalize_return(mnesia:transaction(Fun)).

retrieve_gifs_by_user_id(UserUUID) ->
  Fun = fun() ->
    Query = qlc:q([Gif || Gif <- mnesia:table(gif),
      Gif#gif.user_uuid == UserUUID]),
    qlc:e(Query)
  end,
  normalize_return(mnesia:transaction(Fun)).

retrieve_gifs_by_categories(UserUUID, Categories) ->
  Fun = fun() ->
    Query = qlc:q([Gif || Gif <- mnesia:table(gif),
      (Gif#gif.user_uuid == UserUUID) andalso match_categories(Categories, Gif#gif.categories, false)]),
    qlc:e(Query)
  end,
  normalize_return(mnesia:transaction(Fun)).

match_categories([], _GifCategories, MatchAcc) ->
  MatchAcc;
match_categories([FilterCategory | FilterCategories], GifCategories, MatchAcc) ->
  MatchingCategories = [re:run(GifCategory, FilterCategory, [{capture, none}]) == match || GifCategory <- GifCategories],
  Fun = fun(Match, Acc) -> Match orelse Acc end,
  NewMatchAcc = lists:foldl(Fun, MatchAcc, MatchingCategories),
  match_categories(FilterCategories, GifCategories, NewMatchAcc).

normalize_return({atomic, Value}) -> {ok, Value};
normalize_return({aborted, Reason}) ->
  lager:error("Database error: ~p", [{error, Reason}]),
  {error, Reason}.

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

%% A quick way to seed the mnesia tables for testing. Returns the UserUUID you'll need to retrieve and save gifs
seed_tables_for_testing() ->
  {ok, ok} = insert_user(<<"test_uuid">>, <<"Samwar">>, <<"test">>, <<"samwar@gmail.com">>),
  {ok, User} = retrieve_user(<<"Samwar">>, <<"test">>, true),
  UserUUID = User#user.uuid,
  insert_gif(iolist_to_binary([UserUUID, "_FiGiRei2ICzzG"]), <<"https://media2.giphy.com/media/FiGiRei2ICzzG/giphy.gif">>, [<<"funny">>, <<"cat">>, <<"roomba">>], UserUUID),
  insert_gif(iolist_to_binary([UserUUID, "_feqkVgjJpYtjy"]), <<"https://media0.giphy.com/media/feqkVgjJpYtjy/giphy.gif">>, [<<"bird">>, <<"zoom">>], UserUUID),
  UserUUID.
