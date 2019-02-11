%%%-------------------------------------------------------------------
%%% @author Sam Warters
%%% @doc
%%% Contains record definitions for the mnesia tables
%%% @end
%%% Created : 26. Jan 2019 16:33
%%%-------------------------------------------------------------------

-record(user, {
  uuid      :: binary(),  % Primary Key as deemed by mnesia implementation
  username  :: binary(),  % Indexed
  password  :: binary(),
  email     :: binary()
}).

-record(gif, {
  gif_id     :: binary(), % Primary Key as deemed by mnesia implementation
  giphy_uri  :: binary(), % Indexed
  categories :: binary(),
  user_uuid  :: binary()  % Indexed
}).
