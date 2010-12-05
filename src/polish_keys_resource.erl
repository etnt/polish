%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_resource).

-export([get_list/1, get/2, put/3]).

-include("polish.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A P I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_list([]) ->
  [];
get_list(Query) ->
  do_get_list(generate_complete_query(Query)).

get(ResourceID, User) ->
  {K, V} = read_key(ResourceID),
  IsLocked = is_key_locked(ResourceID, User),
  IsMarkedAsTranslated = is_marked_as_translated(ResourceID),
  {K, V, IsLocked, IsMarkedAsTranslated}.

put(ResourceID, Body, User) ->
  assert_key_exists(ResourceID),
  case polish_server:is_key_locked_by_another_user(ResourceID, User) of
    true  -> {error, locked_key};
    false -> do_put(ResourceID, Body, User)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  I N T E R N A L   F U N C T I O N S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_list
%------------------------------------------------------------------------------
generate_complete_query(Query) ->
  DefaultQuery = get_default_query(),
  Query1 = [{K, V} || {K, V} <- Query, proplists:is_defined(K, DefaultQuery)],
  lists:ukeymerge(1, Query1, DefaultQuery).

get_default_query() ->
  [ {"lang", polish:get_default_lang()}
  , {"offset", "0"}
  , {"translated", "false"}
  , {"untranslated", "true"}
  , {"search_in_key", "true"}
  , {"search_in_value", "false"}
  , {"match_type", "any"}
  , {"string_search", ".*"}].

do_get_list(Query) ->
  KVs = get_partially_matching_keys(Query),
  Keys = get_matching_keys(KVs, Query),
  polish_server:lock_keys(Keys, ?l2a(?lkup("lang", Query))),
  Keys.

get_partially_matching_keys(Query) ->
  LC = ?lkup("lang", Query),
  Translated = ?l2a(?lkup("translated", Query)),
  Untranslated = ?l2a(?lkup("untranslated", Query)),
  get_partially_matching_keys(LC, Translated, Untranslated).

get_partially_matching_keys(LC, false = _Trans, true = _Untrans) ->
  get_partially_matching_keys(LC, fun(K, V) -> (K == V) orelse (V == "") end);
get_partially_matching_keys(LC, true = _Trans, false = _Untrans) ->
  get_partially_matching_keys(LC, fun(K, V) -> K =/= V end);
get_partially_matching_keys(LC, true = _Trans, true = _Untrans) ->
  get_partially_matching_keys(LC, fun(_K, _V) -> true end);
get_partially_matching_keys(LC, false = _Trans, false = _Untrans) ->
  get_partially_matching_keys(LC, fun(_K, _V) -> false end).

get_partially_matching_keys(LC, F) ->
  [{Key,Val} || {Key,Val} <- polish_server:read_po_file(LC), F(Key, Val)].

-define(RESULTS_LIMIT, 20).
get_matching_keys(KVs, Query) ->
  Offset = ?l2i(?lkup("offset", Query)),
  LC = ?l2a(?lkup("lang", Query)),
  SearchInKey = ?l2a(?lkup("search_in_key", Query)),
  SearchInValue = ?l2a(?lkup("search_in_value", Query)),
  MatchType = ?l2a(?lkup("match_type", Query)),
  StringSearch = ?lkup("string_search", Query),
  get_matching_keys(KVs, Offset, ?RESULTS_LIMIT, LC,
		       {StringSearch, SearchInKey, SearchInValue, MatchType}).

get_matching_keys([], _, _, _, _) ->
  [];
get_matching_keys(T, 1, N, LC, S) ->
  get_matching_keys(T, N, LC, S);
get_matching_keys([_H|T], Offset, N, LC, S) ->
  get_matching_keys(T, Offset - 1, N, LC, S).

get_matching_keys([{K,V} = H|T], N, LC, Search) when N > 0 ->
  case (polish_server:is_key_locked(K, LC) orelse
	polish_server:is_always_translated(LC, K)) of
    true                            ->
      get_matching_keys(T, N, LC, Search);
    false when Search =:= no_search ->
      [H|get_matching_keys(T, N-1, LC, Search)];
    false ->
      case match_entry({K, V}, Search) of
	nomatch -> get_matching_keys(T, N, LC, Search);
	match   -> [H|get_matching_keys(T, N-1, LC, Search)]
      end
  end;
get_matching_keys(_, N, _LC, _S) when N =< 0   -> [];
get_matching_keys([], _, _LC, _S)              -> [].

match_entry({K, _V}, {Str, true = _Key, false = _Value, exact = _MatchType}) ->
  run_literal(K, Str);
match_entry({_K, V}, {Str, false = _Key, true = _Value, exact = _MatchType}) ->
  run_literal(V, Str);
match_entry({K, V}, {Str, true = _Key, true = _Value, exact = _MatchType}) ->
  case run_literal(K, Str) of
    nomatch -> run_literal(V, Str);
    _       -> match
  end;
match_entry({K, _V}, {Str, true = _Key, false = _Value, any = _MatchType}) ->
  run_re(K, Str);
match_entry({_K, V}, {Str, false = _Key, true = _Value, any = _MatchType}) ->
  run_re(V, Str);
match_entry({K, V}, {Str, true = _Key, true = _Value, any = _MatchType}) ->
  case run_re(K, Str) of
    nomatch -> run_re(V, Str);
    _       -> match
  end;
match_entry({_K,_V}, _Search) ->
  nomatch.

run_literal(S1, S2) ->
  Exp = polish_utils:trim_whitespace(string:to_lower(S1)),
  Res = polish_utils:trim_whitespace(string:to_lower(S2)),
  case Res == Exp of
    true  -> match;
    false -> nomatch
  end.

%% When the translation is empty (as in "") the V is header_info. Weird...
run_re(header_info, _RegExp) ->
  nomatch;
run_re(V, RegExp0) ->
  RegExp = escape_regexp(RegExp0),
  case re:run(V, RegExp) of
    nomatch -> nomatch;
    _       -> match
  end.

escape_regexp(RegExp)               -> escape_regexp(RegExp, []).
escape_regexp([$$ | RegExp], Acc)   -> escape_regexp(RegExp, [$$,$\\ | Acc]);
escape_regexp([$? | RegExp], Acc)   -> escape_regexp(RegExp, [$?,$\\ | Acc]);
escape_regexp([$* | RegExp], Acc)   -> escape_regexp(RegExp, [$*,$\\ | Acc]);
escape_regexp([$( | RegExp], Acc)   -> escape_regexp(RegExp, [$(,$\\ | Acc]);
escape_regexp([$) | RegExp], Acc)   -> escape_regexp(RegExp, [$),$\\ | Acc]);
escape_regexp([Char | RegExp], Acc) -> escape_regexp(RegExp, [Char | Acc]);
escape_regexp([], Acc)              -> lists:reverse(Acc).


% get
%------------------------------------------------------------------------------
read_key(ResourceID) ->
  case polish_server:try_read_key(ResourceID) of
    false -> throw(bad_uri);
    Res   -> Res
  end.

is_key_locked(ResourceID, User) ->
  case polish_server:is_key_locked(ResourceID) of
    true  -> true;
    false ->
      polish_server:lock_key(ResourceID, User),
      false
  end.

is_marked_as_translated(ResourceID) ->
  polish_server:is_always_translated(ResourceID).


% put
%------------------------------------------------------------------------------
assert_key_exists(ResourceID) ->
  read_key(ResourceID).

do_put(ResourceID, Body, User) ->
  case maybe_save_translation(ResourceID, Body, User) of
    false -> maybe_mark_as_always_translated(ResourceID, Body, User);
    Res   -> Res
  end,
  polish_server:unlock_user_keys(User).

maybe_save_translation(ID, Body, User) ->
  case {lists:keyfind("translation", 1, Body),
	lists:keyfind("bypass_validators", 1, Body)} of
    {{_, Translation}, {_, ByPass}} ->
      save_translation(ID, ?l2a(ByPass), Translation, User);
    {{_, Translation}, false} ->
      save_translation(ID, false, Translation, User);
    _ ->
      false
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

save_translation([LC1, LC2|_] = ID, ByPassValidators, Translation0, User) ->
  {Key, _V} = ?MODULE:get(ID),
  Translation = format_translation(Key, Translation0),
  case validate_translation(Key, Translation, ByPassValidators) of
    {error, _} = Err ->
      Err;
    ValidatedTranslation  ->
      polish_server:write_key(ID, ValidatedTranslation),
      polish_wash:write_po_file([LC1, LC2]),
      log_save_translation([LC1,LC2], User, save,
			   {Key, ValidatedTranslation}),
      ok
  end.

format_translation(Key, Translation) ->
  polish_utils:to_latin1(
    polish_utils:restore_whitespace(
      Key, polish_utils:trim_whitespace(Translation))).

validate_translation(_Key, Translation, _ByPassValidator = true) ->
  Translation;
validate_translation(Key, Translation, _ByPassValidator = false) ->
  case is_correct_translation(Key, Translation) of
    true         -> Translation;
    {false, Err} -> {error, Err}
  end.

is_correct_translation(Key, Val) ->
  Validators = [gettext_validate_bad_ftxt, gettext_validate_bad_stxt,
		gettext_validate_bad_case, gettext_validate_bad_html,
		gettext_validate_bad_punct, gettext_validate_bad_ws],
  run_validators(Key, Val, Validators).

run_validators(_Key, _Val, []) ->
  true;
run_validators(Key, Val, [Validator|T]) ->
  case run_validator(Validator, Key, Val) of
    ok           -> run_validators(Key, Val, T);
    {error, Msg} -> {false, Msg}
  end.

run_validator(Module, K, V) ->
  case Module:check({K, V}, polish_server, []) of
    [] -> ok;
    Err when element(1, hd(Err)) =:= 'ERROR' orelse
	     element(1, hd(Err)) =:= 'Warning' ->
      {error, element(2, hd(Err))}
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
