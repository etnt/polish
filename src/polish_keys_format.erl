%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_format).

-export([list/2, key/3, put/2]).
-import(polish_utils, [to_utf8/1]).

-include("polish.hrl").


list(List, ?JSON) ->
  F = fun({{Key, Translation}, LC, IsLocked, IsAlwaysTrans}) ->
	  {struct, [{url, ?l2b(key_url(Key, LC))},
		    {key, ?l2b(to_utf8(Key))},
		    {translation, ?l2b(to_utf8(Translation))},
		    {locked, ?l2b(?a2l(IsLocked))},
		    {marked_as_translated, ?l2b(?a2l(IsAlwaysTrans))}]} end,
  mochijson2:encode({array, [F(E) || E <- List]});
list(_Data, _CT) ->
  throw(not_supported).

key({Key, Translation, IsLocked, IsAlwaysTrans}, ID, ?JSON) ->
  mochijson2:encode({struct, [{url, ?l2b(key_url(ID))},
			      {key, ?l2b(to_utf8(Key))},
			      {value, ?l2b(to_utf8(Translation))},
			      {locked, ?l2b(?a2l(IsLocked))},
			      {marked_as_translated, ?l2b(?a2l(IsAlwaysTrans))}
			     ]});
key(_Data, _Key, _CT) ->
  throw(not_supported).

key_url(Key, LC) ->
  key_url(polish_utils:generate_key_identifier(Key, ?a2l(LC))).
key_url(Key) ->
  polish_utils:build_url() ++ "/keys/" ++ Key.

put(ok, ?JSON) ->
  mochijson2:encode({struct, [{result, ok}]});
put({error, Err}, ?JSON) ->
  ErrAtom = case is_atom(Err) of
	      true  -> Err;
	      false -> ?l2a(Err)
	    end,
  mochijson2:encode({struct, [{result, error}, {reason, ErrAtom}]});
put(_Data, _CT) ->
  throw(not_supported).
