%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_resource).

-export([get_list/1, get/1, put/2]).

-include("polish.hrl").

get_list(_Query) ->
     [LC || LC <- polish:all_custom_lcs(), LC =/= "a"].

get(ID) ->
    polish_server:read_key(ID).

put(ID, ReqBody) ->
    case lists:keyfind(action, 1, ReqBody) of
	false ->
	    write(ID, lists:keyfind(translation, 1, ReqBody));
	"mark_as_always_translated" ->
	    polish_server:mark_as_always_translated(ID)
    end.

write(ID, Val0) ->
    {Key, _PrevV} = polish_server:read_key(ID),
    Val = polish_utils:to_latin1(
	    polish_utils:restore_whitespace(
	      Key, polish_utils:trim_whitespace(Val0))),
    case polish_po:check_correctness(Key, Val) of
	ok ->
	    %polish_server:unlock_user_keys(),
	    polish_po:write([{Key, Val}]),
	    ok;
	Err ->
	    Err
    end.
