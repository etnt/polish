%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon
-module(polish_test_lib).
-export([send_http_request/4
	 , send_http_request/5
	 , assert_fields_from_response/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/polish.hrl").

start_polish_for_test() ->
    PWD = get_polish_path(),
    application:load(polish),
    application:set_env(polish, po_lang_dir, PWD++"/priv/lang/"),
    application:set_env(polish, ask_replace_keys, false),
    application:set_env(polish, error_logger_mf_file, PWD++"/logs/polish"),
    application:start(polish).

get_polish_path() ->
    PWD0 = lists:takewhile(
	     fun("polish") -> false;
		(_)        -> true
	     end, string:tokens(os:cmd("pwd"), "/")),
    "/" ++ string:join(PWD0++["polish"], "/").

send_http_request(Method, SubURL, Accept, ExpectedCode) when
      (Method =:= get orelse Method =:= delete) andalso is_list(SubURL) ->
    Request = {polish_utils:build_url() ++ SubURL, [{"Accept", Accept}]},
    send_http_request(Method, Request, ExpectedCode).

send_http_request(Method, SubURL, Body, Accept, ExpectedCode) when
      (Method =:= put orelse Method =:= post) andalso is_list(SubURL) ->
    Request = {polish_utils:build_url() ++ SubURL, [{"Accept", Accept}],
	      "application/x-www-form-urlencoded", Body},
    send_http_request(Method, Request, ExpectedCode).

send_http_request(Method, Request, ExpectedCode) ->
    {ok, {{_, Code, _}, _Headrs, Body}} = http:request(Method, Request, [], []),
    ?assertEqual(ExpectedCode, Code),
    Body.

assert_fields_from_response([{FieldName, ExpectedValue} | T], Response)
  when is_integer(ExpectedValue) ->
    assert_field_from_response(FieldName, ExpectedValue, Response),
    assert_fields_from_response(T, Response);
assert_fields_from_response([{FieldName, ExpectedValue} | T], Response)
  when is_list(ExpectedValue) ->
    assert_field_from_response(FieldName, ?l2b(ExpectedValue), Response),
    assert_fields_from_response(T, Response);
assert_fields_from_response([], _Response) ->
    ok.

assert_field_from_response(FieldName, ExpectedValue, Response) ->
    FieldNameBin = ?l2b(FieldName),
    ?assertEqual({FieldNameBin, ExpectedValue},
		 lists:keyfind(FieldNameBin, 1, Response)).
