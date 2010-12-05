%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_login_format).

-export([login/1
	 , login_error/2]).

-include("polish.hrl").


login(?JSON) ->
  ok.

login_error(Reason, ?JSON) ->
  mochijson2:encode({struct, [{login, error},{reason,?l2a(reason(Reason))}]}).

reason(not_allowed) ->
  "user not allowed";
reason(bad_format) ->
  "wrong openid format";
reason(error) ->
  "login error".
