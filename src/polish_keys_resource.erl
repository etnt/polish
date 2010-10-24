%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_resource).

-export([get_list/1, get/1, put/2]).

-include("polish.hrl").

get_list(_Query) ->
     [LC || LC <- polish:all_custom_lcs(), LC =/= "a"].

get(Key) ->
    case polish_server:try_read_key(Key) of
	false -> throw(bad_uri);
	Res   -> Res
    end.

put([LC1, LC2|_] = Key, Body) ->
    case get_and_validate_translation(Key, Body) of
	{ok, Translation} ->
	    polish_server:write_key(Key, Translation),
	    polish_wash:write([LC1, LC2]);
	{Err, _} ->
	    Err
    end.

get_and_validate_translation(Key, Body) ->
    Translation0 = case lists:keyfind("translation", 1, Body) of
		      false ->
			  throw(bad_request);
		      {"translation", T} ->
			  polish_utils:trim_whitespace(T)
		  end,
    {K, _V} = ?MODULE:get(Key),
    Translation = polish_utils:restore_whitespace(K, Translation0),
    {polish_po:check_correctness(K, Translation), Translation}.

    %% Str = polish_utils:build_info_log(?l2a(LC), "", KV),
    %% error_logger:info_msg(Str).

%% put(ID, ReqBody) ->
%%     case lists:keyfind(action, 1, ReqBody) of
%% 	false ->
%% 	    write(ID, lists:keyfind(translation, 1, ReqBody));
%% 	"mark_as_always_translated" ->
%% 	    polish_server:mark_as_always_translated(ID)
%%     end.

%% write(ID, Val0) ->
%%     {Key, _PrevV} = polish_server:read_key(ID),
%%     Val = polish_utils:to_latin1(
%% 	    polish_utils:restore_whitespace(
%% 	      Key, polish_utils:trim_whitespace(Val0))),
%%     case polish_po:check_correctness(Key, Val) of
%% 	ok ->
%% 	    %polish_server:unlock_user_keys(),
%% 	    polish_po:write([{Key, Val}]),
%% 	    ok;
%% 	Err ->
%% 	    Err
%%     end.

