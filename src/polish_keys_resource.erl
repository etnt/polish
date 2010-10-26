%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_resource).

-export([get_list/1, get/1, put/2]).

-include("polish.hrl").

get_list(_Query) ->
     [LC || LC <- polish:all_custom_lcs(), LC =/= "a"].

get(ID) ->
    case polish_server:try_read_key(ID) of
	false -> throw(bad_uri);
	Res   -> Res
    end.

put(ID, Body) ->
    case maybe_save_translation(ID, Body) of
	false -> maybe_mark_as_always_translated(ID, Body);
	Res   -> Res
    end.

maybe_save_translation(ID, Body) ->
    case lists:keyfind("translation", 1, Body) of
	false            -> false;
	{_, Translation} -> save_translation(ID, Translation)
    end.

maybe_mark_as_always_translated(ID, Body) ->
    case lists:keyfind("mark_as_always_translated", 1, Body) of
	false  -> throw(bad_request);
	{_, _} -> polish_server:mark_as_always_translated(ID)
    end.

save_translation([LC1, LC2|_] = ID, Translation) ->
    {Key, _V} = ?MODULE:get(ID),
    case validate_translation(Key, Translation) of
	{error, _} = Err ->
	    Err;
	ValidatedTranslation  ->
	    polish_server:write_key(ID, ValidatedTranslation),
	    polish_wash:write([LC1, LC2]),
	    log_save_translation([LC1,LC2], Key, ValidatedTranslation),
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

log_save_translation(LC, Key, Translation) ->
    Str = polish_utils:build_info_log(?l2a(LC), "", [Key, Translation]),
    error_logger:info_msg(Str).
