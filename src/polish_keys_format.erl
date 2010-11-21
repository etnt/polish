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

key({Key, Translation, IsLocked}, Key, ?JSON) ->
    mochijson2:encode({struct, [{url, key_url(Key)},
				{key, ?l2a(to_utf8(Key))},
				{value, ?l2a(to_utf8(Translation))},
				{locked, IsLocked}]});
key(_Data, _Key, _CT) ->
    throw(not_supported).

key_url(Key) ->
    ?l2a(polish_utils:build_url() ++ "/keys/" ++ Key).

put(ok, ?JSON) ->
    mochijson2:encode({struct, [{result, ok}]});
put({error, Err}, ?JSON) ->
    mochijson2:encode({struct, [{result, error}, {reason, ?l2a(Err)}]});
put(_Data, _CT) ->
    throw(not_supported).
