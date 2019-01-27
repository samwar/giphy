-module(giphy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [{"/", hello_handler, []}]}
	]),
	{ok, _} = cowboy:start_clear(my_http_listener,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
	start_mnesia(),
	giphy_sup:start_link().

stop(_State) ->
	ok.

start_mnesia() ->
	application:ensure_started(mnesia),
	mnesia:create_schema([node()]),
	giphy_table_mngr:create_tables(),
	mnesia:start().

