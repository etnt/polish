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
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% I N I T S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
    polish_test_lib:start_polish_for_test(),
    Config.

init_per_testcase(http_get_key, Config) ->
    [{key, "jag heter POlish"}, {translation, "em dic POlish"}|Config];
init_per_testcase(_TestCase, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% E N D S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_per_suite(_Config) ->
    ok.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% T E S T   C A S E S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
http_get_key(Config) ->
    Key = ?lkup(key, Config),
    ExpectedTranslation = ?lkup(translation, Config),
    ResourceID = polish_utils:generate_key_identifier(Key, "ca"),
    {Code, ResponseJSON} = polish_test_lib:send_http_request(
			     get, "/keys/"++ResourceID, ?JSON),
    ?assertEqual(?OK, Code),
    {struct, Response} = mochijson2:decode(ResponseJSON),
    polish_test_lib:assert_fields_from_response(
      [{"url", polish_utils:build_url()++"/keys/"++ResourceID},
       {"key", Key}, {"value", ExpectedTranslation}], Response),
    ok.

http_bad_method_key(_Config) ->
    {Code1, _} = polish_test_lib:send_http_request(delete, "/keys/ca1", ?JSON),
    ?assertEqual(?BAD_METHOD, Code1),
    {Code2, _} = polish_test_lib:send_http_request(post, "/keys/ca1", ?JSON),
    ?assertEqual(?BAD_METHOD, Code2),
    ok.

http_bad_method_keys(Config) ->
    {Code1, _} = polish_test_lib:send_http_request(delete, "/keys", ?JSON),
    ?assertEqual(?BAD_METHOD, Code1),
    {Code2, _} = polish_test_lib:send_http_request(put,"/keys", "", ?JSON),
    ?assertEqual(?BAD_METHOD, Code2),
    {Code3, _} = polish_test_lib:send_http_request(post,"/keys","", ?JSON),
    ?assertEqual(?BAD_METHOD, Code3),
    ok.

http_not_existent_key(_Config) ->
    {Code, _} = polish_test_lib:send_http_request(get, "/keys/ca435", ?JSON),
    ?assertEqual(?NOT_FOUND, Code),
    ok.
