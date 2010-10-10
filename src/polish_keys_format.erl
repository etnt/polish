%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_format).

-export([list/2, key/3]).
-import(polish_utils, [to_utf8/1]).

-include("polish.hrl").


list(_Data0, "application/json") ->
    Data = [{struct, []}],
    mochijson2:encode({array, Data});
list(_Data, _CT) ->
    throw(not_supported).

key({K, V}, Key, "application/json") ->
    mochijson2:encode({struct, [{url, key_url(Key)},
				{key, ?l2a(to_utf8(K))},
				{value, ?l2a(to_utf8(V))}]});
key(_Data, _Key, _CT) ->
    throw(not_supported).

key_url(Key) ->
    ?l2a(polish_utils:build_url() ++ "/keyss/" ++ Key).
