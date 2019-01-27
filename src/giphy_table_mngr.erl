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
  retrieve_user/2,
  insert_gif/3,
  retrieve_gifs/1
]).


create_tables() ->
  % Create the user table, build an index on username and uuid
  mnesia:create_table(user, [
    {index, [uuid]},
    {attributes, record_info(fields, user)}
  ]),

  % Create the gif table, build an index on user_uuid
  mnesia:create_table(gif, [
    {index, [user_uuid]},
    {attributes, record_info(fields, gif)}
  ]).

insert_user(Username, Password, Email) ->
  User = #user{
    username = Username,
    password = base64:encode(Password),
    email = Email,
    uuid = giphy_helper:uuid_v4()
  },
  Fun = fun() ->
    mnesia:write(User)
        end,
  normalize_return(mnesia:transaction(Fun)).

retrieve_user(Username, Password) ->
  Fun = fun() ->
    Query = qlc:q([User || User <- mnesia:table(user),
      User#user.username == Username]),
    qlc:e(Query)
        end,
  do_retrieve_user(mnesia:transaction(Fun), Password).

do_retrieve_user({atomic, []}, _Password) ->
  {error, not_found};
do_retrieve_user({atomic, [User]}, Password) ->
  EncodedPassword = base64:encode(Password),
  authorized_user(EncodedPassword, User);
do_retrieve_user({abort, _Reason} = Result, _Password) ->
  Result.

authorized_user(Password, #user{password = Password} = User) ->
  % Return the user without the password so it isn't flying around in the UI.
  UserNoPassword = User#user{password = undefined},
  {ok, UserNoPassword};
authorized_user(_EncodedPassword, #user{password = _Password}) ->
  {error, {unauthorized_user, <<"The user is unauthorized">>}}.


insert_gif(GiphyURI, Categories, UserUUID) ->
  Gif = #gif{
    giphy_uri = GiphyURI,
    categories = Categories,
    user_uuid = UserUUID
  },
  Fun = fun() ->
    mnesia:write(Gif)
        end,
  normalize_return(mnesia:transaction(Fun)).

retrieve_gifs(UserUUID) ->
  Fun = fun() ->
    Query = qlc:q([Gif || Gif <- mnesia:table(gif),
      Gif#gif.user_uuid == UserUUID]),
     qlc:e(Query)
        end,
  normalize_return(mnesia:transaction(Fun)).

normalize_return({atomic, Value}) -> {ok, Value};
normalize_return({abort, Reason}) -> {error, Reason}.

