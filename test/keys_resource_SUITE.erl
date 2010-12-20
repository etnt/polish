%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon
-module(keys_resource_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/polish.hrl").

suite() ->
  [].

all() ->
  [http_get_key
   , http_bad_method_key
   , http_bad_method_keys
   , http_not_existent_key
   , http_put_key
  ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% I N I T S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
  polish_test_lib:start_polish_for_test(),
  UserId = "http://jordi-chacon.myopenid.com/",
  Cookie = polish_test_lib:fake_login(UserId),
  [{cookie, Cookie}, {user_id, UserId} | Config].

init_per_testcase(http_get_key, Config) ->
  [{key, "jag heter POlish"}, {translation, "em dic POlish"}|Config];
init_per_testcase(http_put_key, Config) ->
  Path = polish_test_lib:get_polish_path() ++ "/priv/lang/custom/ca/",
  os:cmd("cp " ++ Path ++ "gettext.po " ++ Path ++ "gettext.po.bup"),
  [{key, "jag heter POlish"}|Config];
init_per_testcase(_TestCase, Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% E N D S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_per_suite(_Config) ->
  polish_test_lib:fake_logout(),
  ok.

end_per_testcase(http_put_key, _Config) ->
  Path = polish_test_lib:get_polish_path() ++ "/priv/lang/custom/ca/",
  os:cmd("mv " ++ Path ++ "gettext.po.bup " ++ Path ++ "gettext.po"),
  os:cmd("rm " ++ Path ++ "gettext.po__*"),
  ok;
end_per_testcase(_TestCase, _Config) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% T E S T   C A S E S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
http_get_key(Config) ->
  Key = ?lkup(key, Config),
  Cookie = ?lkup(cookie, Config),
  ExpectedTranslation = ?lkup(translation, Config),
  ResourceID = polish_utils:generate_key_identifier(Key, "ca"),
  {Code, ResponseJSON} = polish_test_lib:send_http_request(
			   get, "/keys/"++ResourceID, [{cookie, Cookie}]),
  ?assertEqual(?OK, Code),
  {struct, Response} = mochijson2:decode(ResponseJSON),
  ?assertEqual(5, length(Response)),
  polish_test_lib:assert_fields_from_response(
    [{"url", polish_utils:build_url()++"/keys/"++ResourceID},
     {"key", Key}, {"value", ExpectedTranslation}, {"locked", "false"},
     {"marked_as_translated", "false"}], Response),
  ok.

http_bad_method_key(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code1, _} = polish_test_lib:send_http_request(
		 delete, "/keys/ca1", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code1),
  {Code2, _} = polish_test_lib:send_http_request(
		 post, "/keys/ca1", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code2),
  ok.

http_bad_method_keys(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code1, _} = polish_test_lib:send_http_request(
		 delete, "/keys", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code1),
  {Code2, _} = polish_test_lib:send_http_request(
		 put, "/keys", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code2),
  {Code3, _} = polish_test_lib:send_http_request(
		 post, "/keys", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code3),
  ok.

http_not_existent_key(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code1, _} = polish_test_lib:send_http_request(
		get, "/keys/ca435", [{cookie, Cookie}]),
  ?assertEqual(?NOT_FOUND, Code1),
  {Code2, _} = polish_test_lib:send_http_request(
		put, "/keys/ca435", [{cookie, Cookie}]),
  ?assertEqual(?NOT_FOUND, Code2),
  ok.

http_put_key(Config) ->
  Key = ?lkup(key, Config),
  Cookie = ?lkup(cookie, Config),
  ResourceID = polish_utils:generate_key_identifier(Key, "ca"),
  %% get the current translation
  {_Code, Response} = do_get_request_on_key(Cookie, ResourceID),
  Translation = ?b2l(?lkup(<<"value">>, Response)),
  %% save a new translation and assert OK
  NewTranslation = Translation ++ "abc",
  Body = "translation="++polish_utils:url_encode(NewTranslation),
  {Code2, _ResponseJSON} = polish_test_lib:send_http_request(
			   put, "/keys/"++ResourceID,
			   [{cookie, Cookie}, {body, Body}]),
  ?assertEqual(?OK, Code2),
  %% check that the po file contains the new translation
  PoFile = gettext:parse_po(polish:po_lang_dir() ++ "custom/ca/gettext.po"),
  ?assertEqual(NewTranslation, ?lkup(Key, PoFile)),
  %% get the key and assert the translation is the new one
  {_Code3, Response2} = do_get_request_on_key(Cookie, ResourceID),
  polish_test_lib:assert_fields_from_response(
    [{"value", NewTranslation}], Response2),
  ok.

do_get_request_on_key(Cookie, ResourceID) ->
  {Code, ResponseJSON} = polish_test_lib:send_http_request(
			   get, "/keys/"++ResourceID, [{cookie, Cookie}]),
  {struct, Response} = mochijson2:decode(ResponseJSON),
  {Code, Response}.
