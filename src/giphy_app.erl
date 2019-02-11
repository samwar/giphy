-module(giphy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", login_handler, []},
			{"/user", user_handler, []},
			{"/search/[:user_uuid]", search_handler, []},
			{"/gifs/[:user_uuid]", gif_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(my_http_listener,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
	start_mnesia(),
	lager:start(),
	giphy_helper:seed_tables_for_testing(),
	giphy_sup:start_link().

stop(_State) ->
	ok.

start_mnesia() ->
	application:ensure_all_started(mnesia),
	mnesia:create_schema([node()]),
	giphy_table_mngr:create_tables(),
	mnesia:start().
