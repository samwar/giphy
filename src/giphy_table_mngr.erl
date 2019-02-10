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
  retrieve_gifs/1
]).


create_tables() ->
  % Create the user table, build an index on username and uuid
  mnesia:create_table(user, [
    {index, [uuid]},
    {attributes, record_info(fields, user)}
  ]),

  % Create the gif table, build an index on giphy_uri and user_uuid
  mnesia:create_table(gif, [
    {index, [giphy_uri, user_uuid]},
    {attributes, record_info(fields, gif)}
  ]).

insert_user(Username, Password, Email) ->
  insert_user(Username, Password, Email, undefined).

insert_user(Username, Password, Email, undefined) ->
  insert_user(Username, Password, Email, list_to_binary(giphy_helper:uuid_v4()));
insert_user(Username, Password, Email, UUID) ->
  User = #user{
    username = Username,
    password = base64:encode(Password), % SUPER DUPER secure base64 encoding
    email = Email,
    uuid = UUID
  },
  Fun = fun() ->
    mnesia:write(User)
  end,
  normalize_return(mnesia:transaction(Fun)).

retrieve_user(Username, Password, AuthCheck) ->
  Fun = fun() ->
    Query = qlc:q([User || User <- mnesia:table(user),
      User#user.username == Username]),
    qlc:e(Query)
        end,
  do_retrieve_user(mnesia:transaction(Fun), Password, AuthCheck).

retrieve_user_by_uuid(UUID) ->
  Fun = fun() ->
    Query = qlc:q([User || User <- mnesia:table(user),
      User#user.uuid == UUID]),
    qlc:e(Query)
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

retrieve_gifs(UserUUID) ->
  Fun = fun() ->
    Query = qlc:q([Gif || Gif <- mnesia:table(gif),
      Gif#gif.user_uuid == UserUUID]),
     qlc:e(Query)
        end,
  normalize_return(mnesia:transaction(Fun)).

normalize_return({atomic, Value}) -> {ok, Value};
normalize_return({aborted, Reason}) ->
  lager:error("Database error: ~p", [{error, Reason}]),
  {error, Reason}.

