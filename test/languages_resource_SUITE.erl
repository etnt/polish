%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon
-module(languages_resource_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/polish.hrl").

suite() ->
    [].

all() ->
    [http_get_languages].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% I N I T S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
    polish_test_lib:start_polish_for_test(),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% E N D S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_per_suite(_Config) ->
    io:format("hola?"),
    ok.
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% T E S T   C A S E S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
http_get_languages(_Config) ->
    ResponseJSON = polish_test_lib:send_http_request(
		     get, "/languages", ?JSON, ?OK),
    Response = mochijson2:decode(ResponseJSON),
    ?assertEqual(3, length(Response)),
    URL = polish_utils:build_url() ++ "/languages",
    [{_,Ca}, {_,En}, {_,Es}] = Response,
    check_url_and_name_of_language(URL ++ "/ca", "Catalan", Ca),
    check_url_and_name_of_language(URL ++ "/en", "English", En),
    check_url_and_name_of_language(URL ++ "/es", "Spanish", Es),
    ok.

check_url_and_name_of_language(ExpectedURL, ExpectedLanguageName, List) ->
    {<<"url">>, URL} = lists:keyfind(<<"url">>, 1, List),
    ?assertEqual(ExpectedURL, ?b2l(URL)),
    {<<"name">>, LanguageName} = lists:keyfind(<<"name">>, 1, List),
    ?assertEqual(ExpectedLanguageName, ?b2l(LanguageName)).
