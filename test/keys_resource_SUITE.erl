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
  , put_key_bad_translation
  , put_key_bad_translation_bypass_validators
  , lock_keys
  , search_basic
  , search_translated
  , search_untranslated
  , search_offset
  , search_string
  ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% I N I T S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
  polish_test_lib:start_polish_for_test(),
  Config.

init_per_testcase(get_key, Config) ->
  common_init() ++ key_init("jag heter POlish") ++
    [{translation, "em dic POlish"} | Config];
init_per_testcase(put_key, Config) ->
  backup_po(),
  common_init() ++ key_init("jag heter POlish") ++ Config;
init_per_testcase(mark_key_as_always_translated, Config) ->
  backup_meta(),
  common_init() ++ key_init("Hej POlish") ++ Config;
init_per_testcase(unmark_key_as_always_translated, Config) ->
  backup_meta(),
  common_init() ++ key_init("POlish") ++ Config;
init_per_testcase(put_key_bad_translation, Config) ->
  common_init() ++ key_init("jag heter POlish") ++ Config;
init_per_testcase(put_key_bad_translation_bypass_validators, Config) ->
  backup_po(),
  common_init() ++ key_init("jag heter POlish") ++ Config;
init_per_testcase(lock_keys, Config) ->
  backup_po(),
  Cookie2 = polish_test_lib:fake_login2("http://etnt.myopenid.com/"),
  common_init() ++ key_init("jag heter POlish") ++ [{cookie2, Cookie2}] ++
    key_init(key2, resource_id2, "Hej POlish") ++ Config;
init_per_testcase(_TestCase, Config) ->
  common_init() ++ Config.

common_init() ->
  UserId = "http://jordi-chacon.myopenid.com/",
  Cookie = polish_test_lib:fake_login(UserId),
  [{cookie, Cookie}, {user_id, UserId}].

key_init(Key) ->
  key_init(key, resource_id, Key).

key_init(K1, K2, Key) ->
  ResourceID = polish_utils:generate_key_identifier(Key, "ca"),
  [{K1, Key}, {K2, ResourceID}].

backup_po() ->
  Path = polish:get_polish_path() ++ "/priv/lang/custom/ca/",
  os:cmd("cp " ++ Path ++ "gettext.po " ++ Path ++ "gettext.po.bup").

backup_meta() ->
  Path = polish:get_polish_path() ++ "/priv/lang/custom/ca/",
  os:cmd("cp " ++ Path ++ "gettext.po.meta " ++ Path ++ "meta.bup").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% E N D S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_per_suite(_Config) ->
  ok.

end_per_testcase(TC, _Config)
  when TC =:= put_key orelse
       TC =:= put_key_bad_translation_bypass_validators orelse
       TC =:= lock_keys ->
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
  [Key, Cookie, ExpectedTranslation, ResourceID] =
    polish_test_lib:config_lkup([key, cookie, translation,resource_id], Config),
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
  [Key, Cookie, ResourceID] =
    polish_test_lib:config_lkup([key, cookie, resource_id], Config),
  %% get the current translation
  {_Code, Response} = do_get_request_on_key(Cookie, ResourceID),
  Translation = ?b2l(?lkup(<<"value">>, Response)),
  %% save a new translation and assert OK
  NewTranslation = Translation ++ "abc",
  save_translation(ResourceID, Cookie, NewTranslation),
  %% check that the po file contains the new translation
  assert_key_translation_in_pofile(Key, NewTranslation),
  %% get the key and assert the translation is the new one
  {_Code3, Response2} = do_get_request_on_key(Cookie, ResourceID),
  polish_test_lib:assert_fields_from_response(
    [{"value", NewTranslation}], Response2),
  ok.

mark_key_as_always_translated(Config) ->
  [Key, Cookie, ResourceID] =
    polish_test_lib:config_lkup([key, cookie, resource_id], Config),
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
  [Key, Cookie, ResourceID] =
    polish_test_lib:config_lkup([key, cookie, resource_id], Config),
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

put_key_bad_translation(Config) ->
  [Key, Cookie, ResourceID] =
    polish_test_lib:config_lkup([key, cookie, resource_id], Config),
  %% get the current translation
  {_Code, Response} = do_get_request_on_key(Cookie, ResourceID),
  Translation = ?b2l(?lkup(<<"value">>, Response)),
  %% save a bad translation (bad punct) and assert error
  assert_bad_translation(ResourceID, Cookie, Translation ++ "abc.", bad_punct),
  %% save a bad translation (bad case) and assert error
  assert_bad_translation(ResourceID, Cookie, "A " ++ Translation, bad_case),
  %% check that the po file contains the old translation
  assert_key_translation_in_pofile(Key, Translation),
  %% get the key and assert the translation is the old one
  {_Code2, Response2} = do_get_request_on_key(Cookie, ResourceID),
  polish_test_lib:assert_fields_from_response(
    [{"value", Translation}], Response2),
  ok.

put_key_bad_translation_bypass_validators(Config) ->
  [Key, Cookie, ResourceID] =
    polish_test_lib:config_lkup([key, cookie, resource_id], Config),
  %% get the current translation
  {_Code, Response} = do_get_request_on_key(Cookie, ResourceID),
  Translation = ?b2l(?lkup(<<"value">>, Response)),
  %% save a bad translation (bad punct) and assert error
  NewTranslation = Translation ++ "abc.",
  assert_bad_translation(ResourceID, Cookie, NewTranslation, bad_punct),
  %% save a bad translation (bad punct) bypassing validator and assert ok
  Body = "translation="++polish_utils:url_encode(NewTranslation) ++
    "&bypass_validators=true",
  {Code, ResponseJSON} = polish_test_lib:send_http_request(
			   put, "/keys/"++ResourceID,
			   [{cookie, Cookie}, {body, Body}]),
  ?assertEqual(?OK, Code),
  {struct, Response2} = mochijson2:decode(ResponseJSON),
  polish_test_lib:assert_fields_from_response([{"result", "ok"}], Response2),
  %% check that the po file contains the new translation
  assert_key_translation_in_pofile(Key, NewTranslation),
  %% get the key and assert the translation is the new one
  {_Code2, Response3} = do_get_request_on_key(Cookie, ResourceID),
  polish_test_lib:assert_fields_from_response(
    [{"value", NewTranslation}], Response3),
  ok.

lock_keys(Config) ->
  [Cookie, Cookie2, Key, ResourceID, ResourceID2] =
    polish_test_lib:config_lkup(
      [cookie, cookie2, key, resource_id, resource_id2], Config),
  %% get the key with user1 (this action locks it)
  {_, Response} = do_get_request_on_key(Cookie, ResourceID),
  polish_test_lib:assert_fields_from_response([{"locked", "false"}], Response),
  Translation = ?b2l(?lkup(<<"value">>, Response)),
  %% get the key with user2 and assert key is locked
  {_, Response2} = do_get_request_on_key(Cookie2, ResourceID),
  polish_test_lib:assert_fields_from_response([{"locked", "true"}], Response2),
  %% try to save a new translation with user2 and get a 'locked_key' error
  NewTranslation = Translation ++ "abc",
  Response3 = save_translation(ResourceID, Cookie2, NewTranslation),
  polish_test_lib:assert_fields_from_response(
    [{"result", "error"}, {"reason", "locked_key"}], Response3),
  %% user1 unlocks key by saving a translation
  NewTranslation2 = Translation ++ "aaaa",
  save_translation(ResourceID, Cookie, NewTranslation2),
  %% assert new translation from user1 has been saved
  assert_key_translation_in_pofile(Key, NewTranslation2),
  %% user2 tries to save a new translation now and it works
  NewTranslation3 = Translation ++ "bbbb",
  Response4 = save_translation(ResourceID, Cookie2, NewTranslation3),
  polish_test_lib:assert_fields_from_response([{"result", "ok"}], Response4),
  %% assert new translation from user2 has been saved
  assert_key_translation_in_pofile(Key, NewTranslation3),

  %% user1 locks the key again
  {_, Response6} = do_get_request_on_key(Cookie, ResourceID),
  polish_test_lib:assert_fields_from_response([{"locked", "false"}], Response6),
  %% user2 tries to get the key and assert key is locked
  {_, Response7} = do_get_request_on_key(Cookie2, ResourceID),
  polish_test_lib:assert_fields_from_response([{"locked", "true"}], Response7),
  %% user2 unlocks the key by doing a get on another key
  {_, Response8} = do_get_request_on_key(Cookie, ResourceID2),
  polish_test_lib:assert_fields_from_response([{"locked", "false"}], Response8),
  %% user2 tries to get the key and assert key is not locked
  {_, Response9} = do_get_request_on_key(Cookie2, ResourceID),
  polish_test_lib:assert_fields_from_response([{"locked", "false"}], Response9),
  ok.

search_basic(Config) ->
  Cookie = ?lkup(cookie, Config),
  %% get catalan keys. All of them are translated or marked as always trans,
  %% so we get an empty list
  Response1 = do_get_request_on_keys(Cookie, "lang=ca"),
  ?assertEqual([], Response1),
  %% get spanish keys. 3 of them are untranslated
  Response2 = do_get_request_on_keys(Cookie, "lang=es"),
  ?assertEqual(3, length(Response2)),
  ok.

search_translated(Config) ->
  Cookie = ?lkup(cookie, Config),
  %% search translated keys and assert they are actually translated
  Translated = do_get_request_on_keys(Cookie, "lang=ca&translated=true"),
  ?assertEqual(true, length(Translated) > 0),
  [?assertEqual(true, ?lkup(<<"key">>, Key) =/= ?lkup(<<"translation">>, Key))
	   || {struct, Key} <- Translated],
  ok.

search_untranslated(Config) ->
  Cookie = ?lkup(cookie, Config),
  %% search untranslated keys and assert they are actually untranslated
  Untranslated = do_get_request_on_keys(Cookie, "lang=es&untranslated=true"),
  ?assertEqual(true, length(Untranslated) > 0),
  [?assertEqual(true, ?lkup(<<"key">>, Key) =:= ?lkup(<<"translation">>, Key))
	   || {struct, Key} <- Untranslated],
  ok.

search_offset(Config) ->
  Cookie = ?lkup(cookie, Config),
  %% get translated keys
  Response1 = do_get_request_on_keys(Cookie, "lang=ca&translated=true"),
  ?assertEqual(true, length(Response1) > 2),
  %% get translated keys with offset 1 and assert result has one key less
  Response2 = do_get_request_on_keys(Cookie,"lang=ca&translated=true&offset=1"),
  ?assertEqual(true, length(Response2) + 1 =:= length(Response1)),
  %% get translated keys with offset 2 and assert result has two keys less
  Response3 = do_get_request_on_keys(Cookie,"lang=ca&translated=true&offset=2"),
  ?assertEqual(true, length(Response3) + 2 =:= length(Response1)),
  %% use too much offset and assert result is empty
  TooBigOffset = ?i2l(length(Response1) + 1),
  Response4 = do_get_request_on_keys(Cookie,"lang=ca&translated=true&offset="
				     ++ TooBigOffset),
  ?assertEqual(0, length(Response4)),
  ok.

search_string(Config) ->
  Cookie = ?lkup(cookie, Config),
  %% search all translated keys that contain 'Hej'. By default it searches
  %% only in keys, not in translations. There should be 1 match
  Query = "lang=ca&translated=true&match_type=any&string_search=Hej",
  Response1 = do_get_request_on_keys(Cookie, Query),
  ?assertEqual(1, length(Response1)),
  %% do the same search specifying now search_in_key=true
  Query2 = "lang=ca&translated=true&match_type=any"
    "&string_search=Hej&search_in_key=true",
  Response2 = do_get_request_on_keys(Cookie, Query2),
  ?assertEqual(1, length(Response2)),
  %% search 'Hola' in the translations. There should be 1 match
  Query3 = "lang=ca&translated=true&match_type=any"
    "&string_search=Hola&search_in_key=false&search_in_value=true",
  Response3 = do_get_request_on_keys(Cookie, Query3),
  ?assertEqual(1, length(Response3)),
  %% Search weird string and match 0
  Query4 = "lang=ca&translated=true&match_type=any&string_search=asdf",
  Response4 = do_get_request_on_keys(Cookie, Query4),
  ?assertEqual(0, length(Response4)),
  %% Search with match_type exact 'Hej POlish' and match 1
  Query5 = "lang=ca&translated=true&&string_search=" ++
    polish_utils:url_encode("Hej POlish") ++
    "&search_in_key=true&match_type=exact",
  Response5 = do_get_request_on_keys(Cookie, Query5),
  ?assertEqual(1, length(Response5)),
  %% Search with match_type exact 'Hej POlis' and match 0
  Query6 = "lang=ca&translated=true&&string_search=" ++
    polish_utils:url_encode("Hej POlis") ++
    "&search_in_key=true&match_type=exact",
  Response6 = do_get_request_on_keys(Cookie, Query6),
  ?assertEqual(0, length(Response6)),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% U T I L S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_bad_translation(ResourceID, Cookie, NewTranslation, Reason) ->
  Response = save_translation(ResourceID, Cookie, NewTranslation),
  polish_test_lib:assert_fields_from_response(
    [{"result", "error"}, {"reason", get_reason(Reason)}],
    Response).

get_reason(bad_punct) ->
  "Trailing punctuation is missmatched.";
get_reason(bad_case) ->
  "Text starts with different case.".

do_get_request_on_key(Cookie, ResourceID) ->
  {Code, ResponseJSON} = polish_test_lib:send_http_request(
			   get, "/keys/"++ResourceID, [{cookie, Cookie}]),
  {struct, Response} = mochijson2:decode(ResponseJSON),
  {Code, Response}.

do_get_request_on_keys(Cookie, Query) ->
  {Code, ResponseJSON} = polish_test_lib:send_http_request(
			   get, "/keys?" ++ Query, [{cookie, Cookie}]),
  ?assertEqual(?OK, Code),
  mochijson2:decode(ResponseJSON).

save_translation(ResourceID, Cookie, NewTranslation) ->
  Body = "translation="++polish_utils:url_encode(NewTranslation),
  {Code, ResponseJSON} = polish_test_lib:send_http_request(
			   put, "/keys/"++ResourceID,
			   [{cookie, Cookie}, {body, Body}]),
  ?assertEqual(?OK, Code),
  {struct, Response} = mochijson2:decode(ResponseJSON),
  Response.

assert_key_translation_in_pofile(Key, Translation) ->
  PoFile = gettext:parse_po(polish:po_lang_dir() ++ "custom/ca/gettext.po"),
  ?assertEqual(Translation, ?lkup(Key, PoFile)).
