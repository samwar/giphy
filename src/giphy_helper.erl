%%%-------------------------------------------------------------------
%%% @author Sam Warters
%%% @doc
%%%
%%% @end
%%% Created : 27. Jan 2019 12:37
%%%-------------------------------------------------------------------
-module(giphy_helper).

%% API
-export([
  uuid_v4/0,
  body/2
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
