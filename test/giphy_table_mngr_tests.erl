%%%-------------------------------------------------------------------
%%% @author Sam Warters
%%% @doc
%%% Some tests for the not so straight forward part of the table manager
%%% @end
%%% Created : 12. Feb 2019 23:01
%%%-------------------------------------------------------------------
-module(giphy_table_mngr_tests).
-include_lib("eunit/include/eunit.hrl").

match_categories_test_() ->
  [
    {
    "'true' is returned if any part of the filter categories match any part of the gif categories'",
    ?_test(begin
      FilterCategories = [<<"dancing">>, <<"talking">>, <<"bananas">>],
      GifCategories = [<<"talking heads">>, <<"david byrne">>, <<"music">>],
      ?assertEqual(true, giphy_table_mngr:match_categories(FilterCategories, GifCategories, false))
    end)
    },
    {
    "'true' is returned if multiple parts of filter categories match any part of the gif categories'",
    ?_test(begin
      FilterCategories = [<<"dancing">>, <<"talking">>, <<"music">>],
      GifCategories = [<<"talking heads">>, <<"david byrne">>, <<"music">>],
      ?assertEqual(true, giphy_table_mngr:match_categories(FilterCategories, GifCategories, false))
    end)
    },
    {
      "'false' is returned if any filter categories do not match any part of the gif categories'",
      ?_test(begin
        FilterCategories = [<<"dancing">>, <<"animals">>, <<"bananas">>],
        GifCategories = [<<"talking heads">>, <<"david byrne">>, <<"music">>],
        ?assertEqual(false, giphy_table_mngr:match_categories(FilterCategories, GifCategories, false))
      end)
    }
  ].
