%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_resource).

-export([get_list/1, get/2, put/3]).

-include("polish.hrl").

get_list(_Query) ->
     [LC || LC <- polish:all_custom_lcs(), LC =/= "a"].

get(ResourceID, User) ->
    {K, V} = read_key(ResourceID),
    IsLocked = case polish_server:is_key_locked(ResourceID) of
		   true  -> true;
		   false ->
		       polish_server:lock_key(ResourceID, User),
		       false
	       end,
    {K, V, IsLocked}.

read_key(ResourceID) ->
    case polish_server:try_read_key(ResourceID) of
	false -> throw(bad_uri);
	Res   -> Res
    end.

put(ResourceID, Body, User) ->
    assert_key_exists(ResourceID),
    case polish_server:is_key_locked_by_another_user(ResourceID, User) of
	true  -> {error, locked_key};
	false -> do_put(ResourceID, Body, User)
    end.

assert_key_exists(ResourceID) ->
    read_key(ResourceID).

do_put(ResourceID, Body, User) ->
    case maybe_save_translation(ResourceID, Body, User) of
	false -> maybe_mark_as_always_translated(ResourceID, Body, User);
	Res   -> Res
    end,
    polish_server:unlock_user_keys(User).

maybe_save_translation(ID, Body, User) ->
    case lists:keyfind("translation", 1, Body) of
	false            -> false;
	{_, Translation} -> save_translation(ID, Translation, User)
    end.

maybe_mark_as_always_translated([LC1,LC2|_] = ID, Body, User) ->
    {K, _} = read_key(ID),
    case lists:keyfind("mark_as_always_translated", 1, Body) of
	false        -> throw(bad_request);
	{_, "true"}  ->
	    polish_server:mark_as_always_translated(ID),
	    log_save_translation([LC1,LC2], User, mark_translated, K);
	{_, "false"} ->
	    polish_server:unmark_as_always_translated(ID),
	    log_save_translation([LC1,LC2], User, unmark_translated, K)
    end.

save_translation([LC1, LC2|_] = ID, Translation, User) ->
    {Key, _V} = ?MODULE:get(ID),
    case validate_translation(Key, Translation) of
	{error, _} = Err ->
	    Err;
	ValidatedTranslation  ->
	    polish_server:write_key(ID, ValidatedTranslation),
	    polish_wash:write_po_file([LC1, LC2]),
	    log_save_translation([LC1,LC2], User, save,
				 {Key, ValidatedTranslation}),
	    ok
    end.

validate_translation(Key, Translation0) ->
    Translation = polish_utils:to_latin1(
		    polish_utils:restore_whitespace(
		      Key, polish_utils:trim_whitespace(Translation0))),
    case polish_po:is_correct_translation(Key, Translation) of
	true         -> Translation;
	{false, Err} -> {error, Err}
    end.

log_save_translation(LC, User, Action, What) ->
    WhatStr = case Action of
		  save ->
		      {K,V} = What,
		      "Key: " ++ K ++ "~nTranslation: " ++ V;
		  mark_translated ->
		      "Key: " ++ What;
		  unmark_translated ->
		      "Key: " ++ What
	      end,
    Str = polish_utils:build_info_log(?l2a(LC), User, Action, WhatStr),
    error_logger:info_msg(Str).
