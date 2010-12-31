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
  [ get_key
  , bad_method_key
  , bad_method_keys
  , not_existent_key
  , put_key
  , mark_key_as_always_translated
  , unmark_key_as_always_translated
  ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% I N I T S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
  polish_test_lib:start_polish_for_test(),
  Config.

init_per_testcase(get_key, Config) ->
  UserId = "http://jordi-chacon.myopenid.com/",
  Cookie = polish_test_lib:fake_login(UserId),
  [{key, "jag heter POlish"}, {translation, "em dic POlish"},
   {cookie, Cookie}, {user_id, UserId}|Config];
init_per_testcase(put_key, Config) ->
  UserId = "http://jordi-chacon.myopenid.com/",
  Cookie = polish_test_lib:fake_login(UserId),
  Path = polish:get_polish_path() ++ "/priv/lang/custom/ca/",
  os:cmd("cp " ++ Path ++ "gettext.po " ++ Path ++ "gettext.po.bup"),
  [{key, "jag heter POlish"}, {cookie, Cookie}, {user_id, UserId}|Config];
init_per_testcase(mark_key_as_always_translated, Config) ->
  UserId = "http://jordi-chacon.myopenid.com/",
  Cookie = polish_test_lib:fake_login(UserId),
  Path = polish:get_polish_path() ++ "/priv/lang/custom/ca/",
  os:cmd("cp " ++ Path ++ "gettext.po.meta " ++ Path ++ "meta.bup"),
  Key = "Hej POlish",
  ResourceID = polish_utils:generate_key_identifier(Key, "ca"),
  [{key, Key}, {resource_id, ResourceID},
   {cookie, Cookie}, {user_id, UserId}| Config];
init_per_testcase(unmark_key_as_always_translated, Config) ->
  UserId = "http://jordi-chacon.myopenid.com/",
  Cookie = polish_test_lib:fake_login(UserId),
  Path = polish:get_polish_path() ++ "/priv/lang/custom/ca/",
  os:cmd("cp " ++ Path ++ "gettext.po.meta " ++ Path ++ "meta.bup"),
  Key = "POlish",
  ResourceID = polish_utils:generate_key_identifier(Key, "ca"),
  [{key, Key}, {resource_id, ResourceID},
   {cookie, Cookie}, {user_id, UserId} | Config];
init_per_testcase(_TestCase, Config) ->
  UserId = "http://jordi-chacon.myopenid.com/",
  Cookie = polish_test_lib:fake_login(UserId),
  [{cookie, Cookie}, {user_id, UserId} | Config].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% E N D S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_per_suite(_Config) ->
  ok.

end_per_testcase(put_key, _Config) ->
  Path = polish:get_polish_path() ++ "/priv/lang/custom/ca/",
  os:cmd("mv " ++ Path ++ "gettext.po.bup " ++ Path ++ "gettext.po"),
  os:cmd("rm " ++ Path ++ "gettext.po__*"),
  polish_test_lib:cleanup(),
  ok;
end_per_testcase(TC, _Config) when TC =:= mark_key_as_always_translated orelse
				   TC =:= unmark_key_as_always_translated ->
  Path = polish:get_polish_path() ++ "/priv/lang/custom/ca/",
  os:cmd("mv " ++ Path ++ "meta.bup " ++ Path ++ "gettext.po.meta"),
  os:cmd("rm " ++ Path ++ "gettext.po__*"),
  polish_test_lib:cleanup(),
  ok;
end_per_testcase(_TestCase, _Config) ->
  polish_test_lib:cleanup(),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% T E S T   C A S E S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_key(Config) ->
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

bad_method_key(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code1, _} = polish_test_lib:send_http_request(
		 delete, "/keys/ca1", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code1),
  {Code2, _} = polish_test_lib:send_http_request(
		 post, "/keys/ca1", [{cookie, Cookie}]),
  ?assertEqual(?BAD_METHOD, Code2),
  ok.

bad_method_keys(Config) ->
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

not_existent_key(Config) ->
  Cookie = ?lkup(cookie, Config),
  {Code1, _} = polish_test_lib:send_http_request(
		get, "/keys/ca435", [{cookie, Cookie}]),
  ?assertEqual(?NOT_FOUND, Code1),
  {Code2, _} = polish_test_lib:send_http_request(
		put, "/keys/ca435", [{cookie, Cookie}]),
  ?assertEqual(?NOT_FOUND, Code2),
  ok.

put_key(Config) ->
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

mark_key_as_always_translated(Config) ->
  Key = ?lkup(key, Config),
  Cookie = ?lkup(cookie, Config),
  ResourceID = ?lkup(resource_id, Config),
  %% check that the key is not marked as always translated in the meta file
  MetaFile = polish:get_polish_path() ++ "/priv/lang/custom/ca/gettext.po.meta",
  {ok, List} = file:consult(MetaFile),
  ?assertEqual(false, lists:member({always_translated, Key}, List)),
  %% mark the key as always translated
  Body = "mark_as_always_translated=true",
  {Code2, _ResponseJSON} = polish_test_lib:send_http_request(
			   put, "/keys/"++ResourceID,
			   [{cookie, Cookie}, {body, Body}]),
  ?assertEqual(?OK, Code2),
  %% check that the key is marked as always translated in the meta file
  {ok, List2} = file:consult(MetaFile),
  ?assertEqual(true, lists:member({always_translated, Key}, List2)),
  %% get the key and assert marked as always translated
  {_Code3, Response2} = do_get_request_on_key(Cookie, ResourceID),
  polish_test_lib:assert_fields_from_response(
    [{"marked_as_translated", "true"}], Response2),
  ok.

unmark_key_as_always_translated(Config) ->
  Key = ?lkup(key, Config),
  Cookie = ?lkup(cookie, Config),
  ResourceID = ?lkup(resource_id, Config),
  %% check that the key is marked as always translated in the meta file
  MetaFile = polish:get_polish_path() ++ "/priv/lang/custom/ca/gettext.po.meta",
  {ok, List} = file:consult(MetaFile),
  ?assertEqual(true, lists:member({always_translated, Key}, List)),
  %% unmark the key as always translated
  Body = "mark_as_always_translated=false",
  {Code2, _ResponseJSON} = polish_test_lib:send_http_request(
			   put, "/keys/"++ResourceID,
			   [{cookie, Cookie}, {body, Body}]),
  ?assertEqual(?OK, Code2),
  %% check that the key is not marked as always translated in the meta file
  {ok, List2} = file:consult(MetaFile),
  ?assertEqual(false, lists:member({always_translated, Key}, List2)),
  %% get the key and assert not marked as always translated
  {_Code3, Response2} = do_get_request_on_key(Cookie, ResourceID),
  polish_test_lib:assert_fields_from_response(
    [{"marked_as_translated", "false"}], Response2),
  ok.

do_get_request_on_key(Cookie, ResourceID) ->
  {Code, ResponseJSON} = polish_test_lib:send_http_request(
			   get, "/keys/"++ResourceID, [{cookie, Cookie}]),
  {struct, Response} = mochijson2:decode(ResponseJSON),
  {Code, Response}.
