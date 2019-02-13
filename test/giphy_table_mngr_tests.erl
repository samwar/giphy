%%%-------------------------------------------------------------------
%%% @author Sam Warters
%%% @doc
%%% Some tests for the not so straight forward part of the table manager
%%% @end
%%% Created : 12. Feb 2019 23:01
%%%-------------------------------------------------------------------
-module(giphy_table_mngr_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/model/giphy_table_definitions.hrl").

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

do_retrieve_user_test_() ->
  [
    {
      "returns '{error, not_found} if mnesia returns an empty list",
      ?_test(begin
        Expected = {error, not_found},
        ?assertEqual(Expected, giphy_table_mngr:do_retrieve_user({atomic, []}, password, auth_check))
      end)
    },
    {
      "skip the auth check and return a user with an undefined password if auth check is false",
      ?_test(begin
        UserUUID = giphy_table_mngr:uuid_v4(),
        User = #user{uuid = UserUUID, username = <<"test">>, password = base64:encode(<<"test">>), email = <<"test@test.com">>},
        Expected = {ok, User#user{password = undefined}},
        ?assertEqual(Expected, giphy_table_mngr:do_retrieve_user({atomic, [User]}, <<"test">>, false))
      end)
    },
    {
      "perform the auth check and return a user with an undefined password if auth check is true and the password is correct",
      ?_test(begin
        UserUUID = giphy_table_mngr:uuid_v4(),
        User = #user{uuid = UserUUID, username = <<"test">>, password = base64:encode(<<"test">>), email = <<"test@test.com">>},
        Expected = {ok, User#user{password = undefined}},
        ?assertEqual(Expected, giphy_table_mngr:do_retrieve_user({atomic, [User]}, <<"test">>, true))
      end)
    },
    {
      "perform the auth check and return an error if auth check is true and the password is incorrect",
      ?_test(begin
        UserUUID = giphy_table_mngr:uuid_v4(),
        User = #user{uuid = UserUUID, username = <<"test">>, password = base64:encode(<<"test">>), email = <<"test@test.com">>},
        Expected = {error, {unauthorized_user, <<"The user is unauthorized">>}},
        ?assertEqual(Expected, giphy_table_mngr:do_retrieve_user({atomic, [User]}, <<"incorrect password">>, true))
      end)
    }
  ].
