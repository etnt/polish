%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon
-module(polish_test_lib).
-export([ send_http_request/3
	, assert_fields_from_response/2
	, write_fake_data_start_auth/1
	, get_fake_redirect_url/1
	, fake_login/1
	, fake_logout/0
	, clean_fake_login_data/0
	, get_polish_path/0]).

-record(preq, { method
	      , url
	      , options
	      , accept
	      , cookie
	      , body
	      , result
	      }).

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

send_http_request(Method, URL, OtherInfo) ->
  Rec = build_request_record(Method, URL, OtherInfo),
  send_http_request(Rec).

build_request_record(Method, URL, OtherInfo) ->
  Rec = #preq{ method  = Method
	     , url     = URL
	     , options = []
	     , cookie  = []
	     , body    = []
	     , result  = basic
	     , accept  = ?JSON},
  fill_record(Rec, OtherInfo).

fill_record(Rec, [{body, Body} | OtherInfo]) ->
  fill_record(Rec#preq{body = Body}, OtherInfo);
fill_record(Rec, [{options, Options} | OtherInfo]) ->
  fill_record(Rec#preq{options = Options}, OtherInfo);
fill_record(Rec, [{cookie, Cookie} | OtherInfo]) ->
  fill_record(Rec#preq{cookie = Cookie}, OtherInfo);
fill_record(Rec, [{result, Result} | OtherInfo]) ->
  fill_record(Rec#preq{result = Result}, OtherInfo);
fill_record(Rec, [{accept, Accept} | OtherInfo]) ->
  fill_record(Rec#preq{accept = Accept}, OtherInfo);
fill_record(Rec, []) ->
  Rec.

send_http_request(Record) ->
  #preq{ method  = Method
       , url     = URL
       , options = Options
       , accept  = Accept
       , cookie  = Cookie
       , body    = Body
       , result  = Result} = Record,
  Request = case Method =:= get orelse Method =:= delete of
	      false ->
		{polish_utils:build_url() ++ URL,
		 [{"Accept", Accept}, {"Cookie", Cookie}],
		 "application/x-www-form-urlencoded", Body};
	      true ->
		{polish_utils:build_url() ++ URL,
		 [{"Accept", Accept}, {"Cookie", Cookie}]}
	    end,
  do_send_http_request(Method, Options, Request, Result).

do_send_http_request(Method, HTTPOptions, Request, What) ->
  {ok, {{_, Code, _}, Headers, Body}} = http:request(Method, Request,
						     HTTPOptions, []),
  case What of
    headers -> {Code, Headers, Body};
    basic   -> {Code, Body}
  end.

assert_fields_from_response([{FieldName, ExpectedValue} | T], Response)
  when is_integer(ExpectedValue) orelse is_atom(ExpectedValue) ->
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

fake_login(UserId) ->
  AuthId = "HMAC-SHA14ce7ff3bchZ2eA",
  [{name, Name}, _] = ?lkup(UserId, polish:get_users()),
  polish_server:write_user_auth(AuthId, Name),
  "auth=" ++ AuthId.

fake_logout() ->
  polish_server:delete_user_auth("HMAC-SHA14ce7ff3bchZ2eA").

write_fake_data_start_auth(UserId) ->
  AuthId = "{HMAC-SHA1}{4ce7ff3b}{chZ2eA==}",
  Data = [{"openid.assoc_handle", AuthId},
	  {"openid.claimed_id", UserId},
	  {"openid.mac_key", <<91,188,209,112,42,145,162,81,9,111,127,179,180,
			       237,117,0,79,174,75,155>>},
	  {"openid.return_to","http://192.168.10.249:8282/auth"},
	  {"openid.server","http://www.myopenid.com/server"},
	  {"openid.trust_root","http://192.168.10.249:8282"},
	  {"openid2.provider","http://www.myopenid.com/server"}],
  polish_server:write_user_auth(AuthId, Data).

clean_fake_login_data() ->
  polish_server:delete_user_auth("HMAC-SHA14ce7ff3bchZ2eA").

get_fake_redirect_url(UserId0) ->
  UserId = polish_utils:url_encode(UserId0),
  "/login?action=auth&openid.assoc_handle=%7BHMAC-SHA1%7D%"
    "7B4ce7ff3b%7D%7BchZ2eA%3D%3D%7D&openid.identity=" ++ UserId ++
    "&openid.mode=id_res&openid.op_endpoint="
    "http%3A%2F%2Fwww.myopenid.com%2Fserver&openid.response_nonce=201"
    "0-11-20T17%3A02%3A53ZnO85X2&openid.return_to=http%3A%2F%2F192.16"
    "8.10.249%3A8282%2Fauth&openid.sig=2l%2BSVA4gD7Faa16RzWY6VBvdk%2F"
    "U%3D&openid.signed=assoc_handle%2Cidentity%2Cmode%2Cop_endpoint%"
    "2Cresponse_nonce%2Creturn_to%2Csigned".
