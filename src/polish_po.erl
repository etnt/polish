%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_po).

-export([  get_entries/1
	 , get_stats/1
        ]).

-include("polish.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A P I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_entries({undefined, _}) ->
  [];
get_entries({LC, Action}) when Action =:= po_file;
			       Action =:= save;
			       Action =:= always_translate ->
  KVs = get_entries_to_edit(LC),
  Entries0 = take(KVs, get_offset() + 20, 21, ?l2a(LC), no_search),
  {Entries, MoreEntries} =
    case length(Entries0) of
      21 -> {tl(Entries0), true};
      _  -> {Entries0, false}
    end,
  {Action, polish_server:lock_keys(Entries, ?l2a(LC)), MoreEntries};
get_entries({LC, {Action0, Str, {Trans, UnTrans, K, V, MatchType}}})
  when Action0 =:= search; Action0 =:= save_search ->
  KVs = get_entries_to_edit(LC, Trans, UnTrans),
  Entries0 = take(KVs, length(KVs), ?l2a(LC), {Str, K, V, MatchType}),
  {Entries, Action} =
    case length(Entries0) > 40 of
      true  -> {element(1,lists:split(40, Entries0)), bad_search};
      false -> {Entries0, Action0}
    end,
  {Action, polish_server:lock_keys(Entries, ?l2a(LC)), false}.

get_stats(undefined) -> {0, 0};
get_stats(LC) ->
  KVs = polish_server:read_po_file(LC),
  Untrans = get_amount_untranslated_keys(KVs, ?l2a(LC)),
  {length(KVs), Untrans}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  I N T E R N A L   F U N C T I O N S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_entries
%------------------------------------------------------------------------------
get_entries_to_edit(LC, {translated, false}, {untranslated, true}) ->
  get_entries_to_edit(LC, fun(K, V) -> (K == V) orelse (V == "") end);
get_entries_to_edit(LC, {translated, true}, {untranslated, false}) ->
  get_entries_to_edit(LC, fun(K, V) -> K =/= V end);
get_entries_to_edit(LC, {translated, true}, {untranslated, true}) ->
  get_entries_to_edit(LC, fun(_K, _V) -> true end);
get_entries_to_edit(LC, {translated, false}, {untranslated, false}) ->
  get_entries_to_edit(LC, fun(_K, _V) -> false end).

get_entries_to_edit(LC) ->
  get_entries_to_edit(LC, fun(K, V) -> (K == V) orelse (V == "") end).

get_entries_to_edit(LC, F) ->
  [{Key,Val} || {Key,Val} <- polish_server:read_po_file(LC), F(Key, Val)].

take([], _, _, _, _)      -> [];
take(T, 1, N, LC, S)      -> take(T, N, LC, S);
take([_H|T], Offset, N, LC, S) -> take(T, Offset - 1, N, LC, S).

take([{K,V} = H|T], N, LC, Search) when N > 0 ->
  case (polish_server:is_key_locked(K, LC) orelse
	polish_server:is_always_translated(LC, K)) of
    true                            ->
      take(T, N, LC, Search);
    false when Search =:= no_search ->
      [H|take(T, N-1, LC, Search)];
    false ->
      case match_entry({K, V}, Search) of
	nomatch -> take(T, N, LC, Search);
	match   -> [H|take(T, N-1, LC, Search)]
      end
  end;
take(_, N, _LC, _S) when N =< 0   -> [];
take([], _, _LC, _S)              -> [].

get_offset() ->
  case wf:session(offset) of
    undefined -> wf:session(offset, 0), 0;
    V         -> V
  end.

match_entry(KV, {Str, Key, Val, {match_type, any}}) ->
  match_entry(KV, {Str, Key, Val});
match_entry({K, _V}, {Str, {key, true}, {value, false}, {match_type, exact}}) ->
  run_literal(K, Str);
match_entry({_K, V}, {Str, {key, false}, {value, true}, {match_type, exact}}) ->
  run_literal(V, Str);
match_entry({K, V}, {Str, {key, true}, {value, true}, {match_type, exact}}) ->
  case run_literal(K, Str) of
    nomatch -> run_literal(V, Str);
    _       -> match
  end;
match_entry({K, _V}, {Str, {key, true}, {value, false}}) ->
  run_re(K, Str);
match_entry({_K, V}, {Str, {key, false}, {value, true}}) ->
  run_re(V, Str);
match_entry({K, V}, {Str, {key, true}, {value, true}}) ->
  case run_re(K, Str) of
    nomatch -> run_re(V, Str);
    _       -> match
  end;
match_entry({_K,_V}, {_S, {key, false}, {value, false}, {match_type, exact}}) ->
  nomatch;
match_entry({_K, _V}, {_Str, {key, false}, {value, false}}) ->
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

escape_regexp(RegExp) ->
  escape_regexp(RegExp, []).
escape_regexp([$$ | RegExp], Acc) -> escape_regexp(RegExp, [$$,$\\ | Acc]);
escape_regexp([$? | RegExp], Acc) -> escape_regexp(RegExp, [$?,$\\ | Acc]);
escape_regexp([$* | RegExp], Acc) -> escape_regexp(RegExp, [$*,$\\ | Acc]);
escape_regexp([$( | RegExp], Acc) -> escape_regexp(RegExp, [$(,$\\ | Acc]);
escape_regexp([$) | RegExp], Acc) -> escape_regexp(RegExp, [$),$\\ | Acc]);
escape_regexp([Char | RegExp], Acc) -> escape_regexp(RegExp, [Char | Acc]);
escape_regexp([], Acc) -> lists:reverse(Acc).


% get_stats
%------------------------------------------------------------------------------
get_amount_untranslated_keys(KVs, LCa) ->
  lists:foldl(
    fun({K, K}, Acc) ->
	case polish_server:is_always_translated(LCa, K) of
	  true  -> Acc;
	  false -> Acc + 1
	end;
       ({_K, _V}, Acc) -> Acc
    end, 0, KVs).
