%%%-------------------------------------------------------------------
%%% @author Sam Warters
%%% @doc
%%% Contains record definitions for the mnesia tables
%%% @end
%%% Created : 26. Jan 2019 16:33
%%%-------------------------------------------------------------------

-record(user, {
  username  :: binary(), % Primary Key as deemed by mnesia implementation
  password  :: binary(),
  email     :: binary(),
  uuid      :: binary()  % Indexed
}).

-record(gif, {
  gif_id     :: binary(), % Primary Key as deemed by mnesia implementation
  giphy_uri  :: binary(), % Indexed
  categories :: binary(),
  user_uuid  :: binary()  % Indexed
}).
