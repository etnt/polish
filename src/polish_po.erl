%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_po).

-export([get_entries/1
	 , get_entry/2
	 , write/0
         , write_po_file/4
	 , get_stats/1
	 , check_correctness/2
	 , update_po_files/2
	 , sort_po_files/1
	 , get_status_po_files/1
	 , get_new_old_keys/1
        ]).

-export([add_new_delete_old_keys/2]).

-import(polish, [a2l/1]).

-import(polish_utils,
        [get_language_name/1
	 , year2str/0
         , rfc3339/0
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A P I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_entries({undefined, _}) ->
    [];
get_entries({LC, Action}) when Action =:= po_file; 
			       Action =:= save;
			       Action =:= always_translate;
			       Action =:= submit ->
    KVs = get_entries_to_edit(LC),
    LCat = list_to_atom(LC),
    Entries0 = take(KVs, get_offset() + 20, 21, LCat, no_search),
    {Entries, MoreEntries} = 
	case length(Entries0) of
	    21 -> {tl(Entries0), true};
	    _  -> {Entries0, false}
	end,
    {Action, polish_server:lock_keys(Entries, LCat), MoreEntries};
get_entries({LC, {Action0, Str, {Trans, UnTrans, K, V, MatchType}}}) 
  when Action0 =:= search; Action0 =:= save_search ->
    KVs = get_entries_to_edit(LC, Trans, UnTrans),
    LCat = list_to_atom(LC),
    Entries0 = take(KVs, length(KVs), LCat, {Str, K, V, MatchType}),
    {Entries, Action} = case length(Entries0) > 40 of
			    true  -> {element(1,lists:split(40, Entries0)), bad_search};
			    false -> {Entries0, Action0}
			end,
    {Action, polish_server:lock_keys(Entries, LCat), false};
get_entries({LC, changes}) ->
    {changes, polish_server:get_changes(list_to_atom(LC)), false}.

get_entry(Key, Info) ->
    lists:keyfind(Key, 1, get_entries(Info)).

write() ->
    LC = wf:session(lang),
    KVs = lists:keysort(1, read_po_file(LC)),
    ok = polish_server:write(KVs, list_to_atom(LC)).

write_po_file(LC, KVs, Name, Email) ->
    Fname = mk_po_filename(LC),
    Tname = Fname++".tmp",
    Bupname = Fname++"__"++rfc3339(),
    mv(Fname, Bupname),
    {ok,Fd} = file:open(Tname, [write]),
    EditPoHeader = polish_deps:get_env(edit_po_header, true),
    write_header(Fd, LC, Name, Email, EditPoHeader),
    write_entries(Fd, KVs),
    file:close(Fd),
    mv(Tname,Fname),
    ok.

get_stats(undefined) -> {0, 0, 0, []};
get_stats(LC) ->
    KVs = read_po_file(LC),
    LCa = list_to_atom(LC),
    Untrans = 
	lists:foldl(
	  fun({K, V}, Acc) ->
		  case K =:= V of
		      true  -> 
			  case polish_server:is_always_translated(LCa, K) of
			      true  -> Acc;
			      false -> Acc + 1
			  end;
		      false -> Acc
		  end
	  end, 0, KVs),
    Trans = polish_server:get_translated_by_country(list_to_atom(LC)),
    Editors = 
	lists:foldl(
	  fun(Editor, Acc) ->
		  case proplists:get_value(Editor, Acc) of
		      undefined -> [{Editor, 1} | Acc];
		      V         -> [{Editor, V+1}|proplists:delete(Editor, Acc)]
		  end
	  end, [], Trans),
    
    {length(KVs), Untrans, length(Trans), Editors}.
    
check_correctness(Key, Val) ->
    F = fun(Module, K, V) ->
		case Module:check({K, V}, polish_server, []) of
		    [] -> ok;
		    [Err] when element(1, Err) =:= 'ERROR' orelse
			       element(1, Err) =:= 'Warning' ->
			{error, element(2, Err)}
		end
	end,
    Validators = [gettext_validate_bad_ftxt, gettext_validate_bad_stxt,
		  gettext_validate_bad_case, gettext_validate_bad_html,
		  gettext_validate_bad_punct, gettext_validate_bad_ws],
    run_validators(F, Key, Val, Validators).

update_po_files(CustomLCs, KeysToBeReplaced0) ->
    %% sort default po file because it may not be completely sorted even
    %% after a make run_gettext. Strings that contain backslashes have problems.
    KeysToBeReplaced = get_keys_to_be_replaced_from_ids(KeysToBeReplaced0),
    sort_po_file(default),
    DefaultPo = read_po_file(default),
    {NewKeys, _} = get_new_old_keys(DefaultPo, hd(CustomLCs)),
    update_po_files(DefaultPo, CustomLCs, KeysToBeReplaced),
    polish_utils:print_email_to_translators(NewKeys).

sort_po_files([LC|CustomLCs]) ->
    sort_po_file(LC),
    sort_po_files(CustomLCs);
sort_po_files([]) ->
    ok.

get_status_po_files(LCs) ->
    DefaultPo = read_po_file(default),
    get_status_po_files(LCs, DefaultPo).

get_new_old_keys(LC) ->
    sort_po_file(default),
    DefaultPo = read_po_file(default),
    NewOld = get_new_old_keys(DefaultPo, LC),
    polish_server:set_new_old_keys(NewOld),
    NewOld.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  I N T E R N A L   F U N C T I O N S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% FIXME insert the proper user info into the header...
write_header(Fd, LC, Name, Email, EditPoHeader) ->
    OrgName = polish:get_org_name(),
    Info = case EditPoHeader of
	       true ->
		   "\"PO-Revision-Date: "++rfc3339()++"\\n\"\n"
		       "\"Last-Translator: "++Name++" <"++Email++">\\n\"\n";
	       false ->
		   "\"Last-Translator: Polish\\n\"\n"
	   end,
    io:format(Fd,
	      "# "++OrgName++" PO file for "++get_language_name(LC)++"\n"
	      "# Copyright (C) "++year2str()++" "++OrgName++"\n"
	      "#\n"
	      "msgid \"\"\n"
	      "msgstr \"\"\n"
	      "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"
	      "\"POT-Creation-Date: 2006-07-01 16:45+0200\\n\"\n"
	      ++ Info ++
	      "\"Language-Team: Klarna <info@klarna.com>\\n\"\n"
	      "\"MIME-Version: 1.0\\n\"\n"
	      "\"Content-Type: text/plain; charset=iso-8859-1\\n\"\n"
	      "\"Content-Transfer-Encoding: 8bit\\n\"\n",
	      []).

    
write_entries(Fd, KVs) ->
    F = fun({header_info,_Val}) ->
		ok;
	   ({Key, Val}) ->
		file:write(Fd, "\nmsgid \"\"\n"),
		write_pretty(Key, Fd),
		file:write(Fd, "msgstr \"\"\n"),
		write_pretty(Val, Fd)
	end,
    lists:foreach(F, KVs).


-define(ENDCOL, 72).
-define(PIVOT, 4).
-define(SEP, $\s).

write_pretty([], _) ->
    true;
write_pretty(Str, Fd) when length(Str) =< ?ENDCOL ->
    write_string(Str, Fd);
write_pretty(Str, Fd) ->
    {Line, Rest} = get_line(Str),
    write_string(Line, Fd),
    write_pretty(Rest, Fd).

write_string(Str, Fd) ->
    file:write(Fd, "\""),
    file:write(Fd, escape_chars(Str)),
    file:write(Fd, "\"\n").

escape_chars(Str) ->
    F = fun($", Acc)  -> [$\\,$"|Acc];
           ($\\, Acc) -> [$\\,$\\|Acc];
           ($\n, Acc) -> [$\\,$n|Acc];
	   (C, Acc)   -> [C|Acc] 
	end,
    lists:foldr(F, [], Str).

%%% Split the string into substrings, 
%%% aligned around a specific column.
get_line(Str) ->
    get_line(Str, ?SEP, 1, ?ENDCOL, []).

%%% End of string reached.
get_line([], _Sep, _N, _End, Acc) ->
    {lists:reverse(Acc), []};
%%% Weird header_info sometimes
get_line(header_info, _Sep, _N, _End, _Acc) ->
    {"", []};
%%% Eat characters.
get_line([H|T], Sep, N, End, Acc) when N < End ->
    get_line(T, Sep, N+1, End, [H|Acc]);
%%% Ended with a Separator on the End boundary.
get_line([Sep|T], Sep, End, End, Acc) ->
    {lists:reverse([Sep|Acc]), T};
%%% At the end, try to find end of token within
%%% the given constraint, else backup one token.
get_line([H|T] = In, Sep, End, End, Acc) ->
    case find_end(T, Sep) of
	{true, Racc, Rest} ->
	    {lists:reverse(Racc ++ [H|Acc]), Rest};
	false ->
	    case reverse_tape(Acc, In) of
		{true, Bacc, Rest} ->
		    {lists:reverse(Bacc), Rest};
		{false,Str} ->
		    %%% Ugh...the word is longer than ENDCOL...
		    split_string(Str, ?ENDCOL)
	    end
    end.

find_end(Str, Sep) ->
    find_end(Str, Sep, 1, ?PIVOT, []).

find_end([Sep|T], Sep, N, Pivot, Acc) when N =< Pivot -> {true, [Sep|Acc], T};
find_end(_Str, _Sep, N, Pivot, _Acc) when N > Pivot   -> false;
find_end([H|T], Sep, N, Pivot, Acc)                   -> find_end(T,Sep,N+1,
                                                                  Pivot,[H|Acc]);
find_end([], _Sep, _N, _Pivot, Acc)                   -> {true, Acc, []}.
    
reverse_tape(Acc, Str) ->
    reverse_tape(Acc, Str, ?SEP).

reverse_tape([Sep|_T] = In, Str, Sep) -> {true, In, Str};
reverse_tape([H|T], Str, Sep)         -> reverse_tape(T, [H|Str], Sep);
reverse_tape([], Str, _Sep)           -> {false, Str}.

split_string(Str, End) ->
    split_string(Str, End, 1, []).

split_string(Str, End, End, Acc)            -> {lists:reverse(Acc), Str};
split_string([H|T], End, N, Acc) when N<End -> split_string(T,End,N+1,[H|Acc]);
split_string([], _End, _N, Acc)             -> {lists:reverse(Acc), []}.


mv(Tname, Fname) ->
    os:cmd("mv "++Tname++" "++Fname).

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
    [{Key,Val} || {Key,Val} <- read_po_file(LC), F(Key, Val)].

take([], _, _, _, _)      -> [];
take(T, 1, N, LC, S)      -> take(T, N, LC, S);
take([_H|T], Offset, N, LC, S) -> take(T, Offset - 1, N, LC, S).

take([{K,V} = H|T], N, LC, Search) when N > 0 -> 
    case (polish_server:is_translated(K, LC) orelse
          polish_server:is_key_locked(K, LC) orelse
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

read_po_file(LC) ->
    try 
	case gettext:parse_po(mk_po_filename(LC)) of
	    []    -> [];
	    [_|T] -> T
	end
    catch
	error:{badmatch, {error, enoent}} -> []
    end.	    

mk_po_filename(LC) ->
    Dir = polish:po_lang_dir(),
    CustomDefaultDir = get_custom_or_default_dir(LC),
    LangDir = get_lang_dir(LC),
    filename:join([Dir, CustomDefaultDir, LangDir,"gettext.po"]).

get_custom_or_default_dir(default) -> "default";
get_custom_or_default_dir(_LC)     -> "custom".

get_lang_dir(default) -> polish:get_default_lang();
get_lang_dir(LC)      -> LC.

match_entry(KV, {Str, Key, Val, {match_type, match_any_word}}) ->
    match_entry(KV, {Str, Key, Val});
match_entry({K, _V}, {Str, {key, true}, {value, false}, {match_type, match_exact_phrase}}) ->
    run_literal(K, Str);
match_entry({_K, V}, {Str, {key, false}, {value, true}, {match_type, match_exact_phrase}}) ->
    run_literal(V, Str);
match_entry({K, V}, {Str, {key, true}, {value, true}, {match_type, match_exact_phrase}}) ->
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
match_entry({_K, _V}, {_Str, {key, false}, {value, false}, {match_type, match_exact_phrase}}) ->
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

run_validators(_F, _Key, _Val, []) ->
    ok;
run_validators(F, Key, Val, [Validator|T]) ->
    case F(Validator, Key, Val) of
	ok                -> run_validators(F, Key, Val, T);
	{error, _Msg} = E -> E
    end.

update_po_files(DefaultPo, [LC|T], KeysToBeReplaced) ->
    case read_and_check_po_file(LC) of
	{duplicated, _D} = Duplicated -> 
	    Duplicated;
	LCPo ->
	    wash_po_file(LCPo, DefaultPo, LC, KeysToBeReplaced),
	    update_po_files(DefaultPo, T, KeysToBeReplaced)
    end;
update_po_files(_DefaultPo, [], _) ->
    ok.

read_and_check_po_file(LC) ->
    LCPo = read_po_file(LC),
    case check_no_duplicates_and_sorted(LCPo, LC) of
	unsorted ->
	    sort_po_file(LC),
	    read_and_check_po_file(LC);
	{duplicated, D} = Duplicated ->
	    error_logger:info_msg(polish:i2l(length(D))++" duplicated keys "
				  "in "++LC++".~n~n"),
	    Duplicated;
	ok ->
	    LCPo
    end.    

get_amount_translated_and_untranslated(LC) ->
    F = fun({K, K}, {Trans, Untrans})   -> {Trans, Untrans + 1};
	   ({_K, _V}, {Trans, Untrans}) -> {Trans + 1, Untrans} end,
    lists:foldl(F, {0, 0}, LC).

check_no_duplicates_and_sorted([], _LC) ->
    ok;
check_no_duplicates_and_sorted([{K,_V}|KVs], LC) ->
    check_no_duplicates_and_sorted(KVs, K, LC, []).
check_no_duplicates_and_sorted([{K,_V}|KVs], PrevK, LC, Duplicated) ->
    case K > PrevK of
	true  -> 
	    check_no_duplicates_and_sorted(KVs, K, LC, Duplicated);
	false when K =:= PrevK -> 
	    check_no_duplicates_and_sorted(KVs, K, LC, [K|Duplicated]);
	false ->
	    unsorted
    end;
check_no_duplicates_and_sorted([], _PrevK, _LC, []) ->
    ok;
check_no_duplicates_and_sorted([], _PrevK, _LC, Duplicated) ->
    {duplicated, Duplicated}.

wash_po_file(PoToWash, DefaultPo, LC, KeysToBeReplaced) ->
    {OldTrans, OldUntrans} = get_amount_translated_and_untranslated(PoToWash),
    PoWashed0 = update_keys_to_be_replaced(PoToWash, KeysToBeReplaced),
    {PoWashed, New, RemovedUntrans, RemovedTrans} = 
	add_new_delete_old_keys(PoWashed0, DefaultPo),
    {NewTrans, NewUntrans} = get_amount_translated_and_untranslated(PoWashed),
    case NewUntrans =:= OldUntrans + New - RemovedUntrans andalso
	NewTrans =:= OldTrans - RemovedTrans of
	true  -> ok;
	false -> 
	    error_logger:error_msg("Bad washing of "++LC++". "
				   "Po file got corrupted"),
	    exit(error)
    end,
    case PoToWash =:= PoWashed of
	false -> 
	    error_logger:info_msg("Updating "++LC++"...~n"),
	    write_po_file(LC, PoWashed, "Polish tool", "polish@polish.org");
	true  ->
            error_logger:info_msg("Nothing to update for "++LC++"...~n"),
            ok
    end.

update_keys_to_be_replaced(PoToWash, KeysToBeReplaced) ->
    F = fun({K,V}, Acc) -> case lists:keyfind(K, 2, KeysToBeReplaced) of
			       false                  -> [{K,V}|Acc];
			       {NewK, K} when K =:= V -> [{NewK, NewK}|Acc];
			       {NewK, K}              -> [{NewK, V}|Acc]
			   end end,
    lists:keysort(1, lists:foldl(F, [], PoToWash)).			
    
    
add_new_delete_old_keys(PoToWash, DefPo) ->
    add_new_delete_old_keys(PoToWash, DefPo, {[], 0, 0, 0}).
add_new_delete_old_keys([{K, _V1} = KV|PoToWash], [{K, _V2}|DefPo], 
			{Acc, New, RU, RT}) ->
    add_new_delete_old_keys(PoToWash, DefPo, {[KV|Acc], New, RU, RT});
add_new_delete_old_keys([{K1, _V1}|_] = PoToWash, [{K2, _V2} = KV|DefPo], 
			{Acc, New, RU, RT}) when K1 > K2 ->
    add_new_delete_old_keys(PoToWash, DefPo, {[KV|Acc], New + 1, RU, RT});
add_new_delete_old_keys([{K, K}|PoToWash], DefPo, 
			{Acc, New, RU, RT}) ->
    add_new_delete_old_keys(PoToWash, DefPo, {Acc, New, RU + 1, RT});
add_new_delete_old_keys([{_K, _V}|PoToWash], DefPo, 
			{Acc, New, RU, RT}) ->
    add_new_delete_old_keys(PoToWash, DefPo, {Acc, New, RU, RT + 1});
add_new_delete_old_keys([], [KV|DefPo], {Acc, New, RU, RT}) ->
    add_new_delete_old_keys([], DefPo, {[KV|Acc], New + 1, RU, RT});
add_new_delete_old_keys([], [], {Acc, New, RU, RT}) ->
    {lists:reverse(Acc), New, RU, RT}.

sort_po_file(LC) ->
    LCPo = read_po_file(LC),
    SortedPo = lists:keysort(1, LCPo),
    case SortedPo =:= LCPo of
	true  ->
            error_logger:info_msg(a2l(LC)++" po file already sorted!"),
            ok;
	false ->
	    error_logger:info_msg(a2l(LC)++" po file not sorted. Sorting..."),
	    write_po_file(LC, SortedPo, "Polish tool", "polish@polish.org")
    end.

is_sorted(KVs, LC) ->
    {_,Res} = lists:foldl(fun({K,_}, {PrevK, Acc}) ->
				  case K > PrevK of
				      true  -> {K, Acc};
				      false -> {K, false}
				  end
			  end, {"", true}, KVs),
    case Res of
	true  -> io:format("~p is sorted~n", [get_lang_dir(LC)]);
	false -> io:format("~p is NOT sorted~n", [get_lang_dir(LC)])
    end,
    Res.

get_status_po_files(LCs, DefaultPo) ->
    LCs2 = [default|LCs],
    ResSorted = check_sorted(LCs2),
    ResLength = check_same_length(LCs2),
    ResKeys = check_same_keys(LCs, DefaultPo),
    ResDuplicated = check_duplicated_keys(LCs2),
    [{sorted, ResSorted}, {length, ResLength}, {same_keys_as_default, ResKeys},
     {not_duplicated_keys, ResDuplicated}].

check_sorted(LCs) ->
    check_sorted(LCs, []).
check_sorted([LC|LCs], Acc) ->
    Res = is_sorted(read_po_file(LC), LC),
    check_sorted(LCs, [{LC,Res}|Acc]);
check_sorted([], Acc) ->
    io:format("~n~n"),
    lists:reverse(Acc).

check_same_length(LCs) ->
    check_same_length(LCs, []).
check_same_length([LC|LCs], Acc) ->
    Length = length(read_po_file(LC)),
    io:format("~p length: ~p~n", [get_lang_dir(LC), Length]),
    check_same_length(LCs, [{LC,Length}|Acc]);
check_same_length([], Acc) ->
    io:format("~n~n"),
    lists:reverse(Acc).

check_same_keys(LCs, DefaultPo) ->
    check_same_keys(LCs, DefaultPo, []).
check_same_keys([LC|LCs], DefaultPo, Acc) ->
    LCPo = read_po_file(LC),
    case check_same_keys_lc(LCPo, DefaultPo) of
	true -> 
	    io:format("~p has same keys as default~n", [get_lang_dir(LC)]),
	    check_same_keys(LCs, DefaultPo, [{LC, true}|Acc]);
	false ->
	    io:format("~p has NOT same keys as default~n", [get_lang_dir(LC)]),
	    check_same_keys(LCs, DefaultPo, [{LC, false}|Acc])
    end;
check_same_keys([], _DefaultPo, Acc) ->
    io:format("~n~n"),
    lists:reverse(Acc).
    
check_same_keys_lc([{K, _}|LCPo], [{K, _}|DefaultPo]) ->
    check_same_keys_lc(LCPo, DefaultPo);
check_same_keys_lc([], []) ->
    true;
check_same_keys_lc(_, _) ->
    false.

check_duplicated_keys(LCs) ->
    check_duplicated_keys(LCs, []).
check_duplicated_keys([LC|LCs], Acc) ->
    [{FirstK, _}|LCPo] = read_po_file(LC),
    case check_duplicated_keys_lc(LCPo, FirstK) of
	ok -> 
	    io:format("~p does not have duplicated keys~n", [get_lang_dir(LC)]),
	    check_duplicated_keys(LCs, [{LC, true}|Acc]);
	duplicated ->
	    io:format("~p has duplicated keys~n", [get_lang_dir(LC)]),
	    check_duplicated_keys(LCs, [{LC, false}|Acc])
    end;
check_duplicated_keys([], Acc) ->
    io:format("~n~n"),
    lists:reverse(Acc).

check_duplicated_keys_lc([{K,_}|LCPo], PrevK) ->
    case PrevK > K of
	true  -> duplicated;
	false -> check_duplicated_keys_lc(LCPo, K)
    end;
check_duplicated_keys_lc([], _) ->
    ok.
	    
get_new_old_keys(KVDef, LC) ->
    KVCus = read_po_file(LC),
    get_new_old_keys(KVDef, KVCus, {[], []}).
get_new_old_keys([{K,_}|KVDef], [{K,_}|KVCus], Acc) -> 
    get_new_old_keys(KVDef, KVCus, Acc);
get_new_old_keys([{K1,_}|_] = KVDef, [{K2,_}|KVCus], {New, Old}) when K1 > K2 -> 
    get_new_old_keys(KVDef, KVCus, {New, [K2|Old]});
get_new_old_keys([{K1,_}|KVDef], KVCus, {New, Old}) -> 
    get_new_old_keys(KVDef, KVCus, {[K1|New], Old});
get_new_old_keys([], _KVCus, Acc) ->
    Acc.

get_keys_to_be_replaced_from_ids(KeysToBeReplaced) ->
    case polish_server:get_new_old_keys() of
	undefined -> 
	    [];
	NewOldKeys -> 
	    get_keys_to_be_replaced_from_ids(KeysToBeReplaced, NewOldKeys, [])
    end.
get_keys_to_be_replaced_from_ids([{NewId, OldId}|KeysToBeReplaced], 
				 {NewKeys, OldKeys} = NLK, Acc) ->
    
    NewKey = lists:nth(NewId, NewKeys),
    OldKey = lists:nth(OldId, OldKeys),
    get_keys_to_be_replaced_from_ids(KeysToBeReplaced, NLK, [{NewKey,OldKey}|Acc]);
get_keys_to_be_replaced_from_ids([], _NLK, Acc) ->
    Acc.
