%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_format).

-export([list/2, key/3, put/2]).
-import(polish_utils, [to_utf8/1]).

-include("polish.hrl").


list(_Data0, ?JSON) ->
  Data = [{struct, []}],
  mochijson2:encode({array, Data});
list(_Data, _CT) ->
  throw(not_supported).

key({Key, Translation, IsLocked, IsMarkedAsTranslated}, ID, ?JSON) ->
  mochijson2:encode({struct, [{url, key_url(ID)},
			      {key, ?l2a(to_utf8(Key))},
			      {value, ?l2a(to_utf8(Translation))},
			      {locked, IsLocked},
			      {marked_as_translated, IsMarkedAsTranslated}]});
key(_Data, _Key, _CT) ->
  throw(not_supported).

key_url(Key) ->
  ?l2a(polish_utils:build_url() ++ "/keys/" ++ Key).

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
