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
  [get_languages,
   get_language,
   bad_method_languages,
   bad_method_language,
   not_existent_language].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% I N I T S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
  polish_test_lib:start_polish_for_test(),
  UserId = "http://jordi-chacon.myopenid.com/",
  Cookie = polish_test_lib:fake_login(UserId),
  [{cookie, Cookie}, {user_id, UserId} | Config].

init_per_testcase(_TestCase, Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% E N D S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_per_suite(_Config) ->
  polish_test_lib:fake_logout(),
  ok.

end_per_testcase(_TestCase, _Config) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% T E S T   C A S E S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_languages(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code, ResponseJSON} = polish_test_lib:send_http_request(
			   get, "/languages", [{cookie, Cookie}]),
  ?assertEqual(?OK, Code),
  Response = mochijson2:decode(ResponseJSON),
  ?assertEqual(3, length(Response)),
  URL = polish_utils:build_url() ++ "/languages",
  [{_,Ca}, {_,En}, {_,Es}] = Response,
  polish_test_lib:assert_fields_from_response(
    [{"url", URL++"/ca"}, {"name", "Catalan"}], Ca),
  polish_test_lib:assert_fields_from_response(
    [{"url", URL++"/en"}, {"name", "English"}], En),
  polish_test_lib:assert_fields_from_response(
    [{"url", URL++"/es"}, {"name", "Spanish"}], Es),
  ok.

get_language(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code, ResponseJSON} = polish_test_lib:send_http_request(
			   get, "/languages/ca", [{cookie, Cookie}]),
  ?assertEqual(?OK, Code),
  {struct, Response} = mochijson2:decode(ResponseJSON),
  polish_test_lib:assert_fields_from_response(
    [{"total", 5}, {"untrans", 0}], Response),
  ok.

bad_method_languages(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code1, _} = polish_test_lib:send_http_request(
		 delete, "/languages", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code1),
  {Code2, _} = polish_test_lib:send_http_request(
		 put, "/languages", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code2),
  {Code3, _} = polish_test_lib:send_http_request(
		 post, "/languages", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code3),
  ok.

bad_method_language(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code1, _} = polish_test_lib:send_http_request(
		 delete, "/languages/ca", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code1),
  {Code2, _} = polish_test_lib:send_http_request(
		 put, "/languages/ca", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code2),
  {Code3, _} = polish_test_lib:send_http_request(
		 post, "/languages/ca", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code3),
  ok.

not_existent_language(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code, _} = polish_test_lib:send_http_request(
		get, "/languages/nn", [{cookie, Cookie}]),
  ?assertEqual(?NOT_FOUND, Code),
  ok.
