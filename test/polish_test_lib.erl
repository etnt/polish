%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon
-module(polish_test_lib).
-export([send_http_request/4
	 , send_http_request/5
	 , send_http_request/6
	 , assert_fields_from_response/2
	 , write_fake_login_data/0
	 , clean_fake_login_data/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/polish.hrl").

start_polish_for_test() ->
    PWD = get_polish_path(),
    application:load(polish),
    application:set_env(polish, po_lang_dir, PWD++"/priv/lang/"),
    application:set_env(polish, ask_replace_keys, false),
    application:set_env(polish, error_logger_mf_file, PWD++"/logs/polish"),
    application:set_env(polish, port, 8283),
    application:start(polish).

get_polish_path() ->
    PWD0 = lists:takewhile(
	     fun("polish") -> false;
		(_)        -> true
	     end, string:tokens(os:cmd("pwd"), "/")),
    "/" ++ string:join(PWD0++["polish"], "/").

send_http_request(Method, HTTPOptions, SubURL, Accept) ->
    send_http_request(Method, HTTPOptions, SubURL, Accept, basic).

send_http_request(Method, HTTPOptions, SubURL, Accept, What)
  when Method =:= get orelse Method =:= delete ->
    Request = {polish_utils:build_url() ++ SubURL, [{"Accept", Accept}]},
    do_send_http_request(Method, HTTPOptions, Request, What);
send_http_request(Method, HTTPOptions, SubURL, Accept, What)
  when Method =:= put orelse Method =:= post ->
    send_http_request(Method, HTTPOptions, SubURL, [], Accept, What).

send_http_request(Method, HTTPOptions, SubURL, Body, Accept, What) ->
    Request = {polish_utils:build_url() ++ SubURL, [{"Accept", Accept}],
	      "application/x-www-form-urlencoded", Body},
    do_send_http_request(Method, HTTPOptions, Request, What).

do_send_http_request(Method, HTTPOptions, Request, What) ->
    {ok, {{_, Code, _}, Headers, Body}} = http:request(Method, Request,
						       HTTPOptions, []),
    case What of
	headers -> {Code, Headers, Body};
	basic   -> {Code, Body}
    end.

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

write_fake_login_data() ->
    AuthId = "{HMAC-SHA1}{4ce7ff3b}{chZ2eA==}",
    Data = [{"openid.assoc_handle", AuthId},
	    {"openid.claimed_id", "http://jordi-chacon.myopenid.com/"},
	    {"openid.mac_key", <<91,188,209,112,42,145,162,81,9,111,127,179,180,
				 237,117,0,79,174,75,155>>},
	    {"openid.return_to","http://192.168.10.249:8282/auth"},
	    {"openid.server","http://www.myopenid.com/server"},
	    {"openid.trust_root","http://192.168.10.249:8282"},
	    {"openid2.provider","http://www.myopenid.com/server"}],
    polish_server:write_user_auth(AuthId, Data).

clean_fake_login_data() ->
    polish_server:delete_user_auth("HMAC-SHA14ce7ff3bchZ2eA").
