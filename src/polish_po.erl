%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_po).

-export([get_entries/1
	 , write/0
         , write_po_file/4
	 , get_stats/1
	 , check_correctness/2
	 , update_po_files/1
	 , sort_po_files/1
        ]).

-export([add_new_delete_old_keys/2]).

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
get_entries({LC, po_file}) ->
    KVs = get_entries_to_edit(LC),
    LCat = list_to_atom(LC),
    Entries = take(KVs, get_offset() + 20, 20, LCat, no_search),
    {translate, polish_server:lock_keys(Entries, LCat)};
get_entries({LC, {search, Str, {Trans, UnTrans, K, V}}}) ->
    KVs = get_entries_to_edit(LC, Trans, UnTrans),
    LCat = list_to_atom(LC),
    Entries = take(KVs, get_offset() + 20, 20, LCat, {Str, K, V}),
    {translate, polish_server:lock_keys(Entries, LCat)};
get_entries({LC, changes}) ->
    {changes, polish_server:get_changes(list_to_atom(LC))}.

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
    write_header(Fd, LC, Name, Email),
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

update_po_files(CustomLCs) ->
    %% sort default po file because it may not be completely sorted even
    %% after a make run_gettext. Strings that contain backslashes have problems.
    sort_po_file(default),
    DefaultPo = read_po_file(default),
    update_po_files(DefaultPo, CustomLCs).

sort_po_files([LC|CustomLCs]) ->
    sort_po_file(LC),
    sort_po_files(CustomLCs);
sort_po_files([]) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  I N T E R N A L   F U N C T I O N S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% FIXME insert the proper user info into the header...
write_header(Fd, LC, Name, Email) ->
    OrgName = polish:get_org_name(),
    io:format(Fd,
	      "# "++OrgName++" PO file for "++get_language_name(LC)++"\n"
	      "# Copyright (C) "++year2str()++" "++OrgName++"\n"
	      "#\n"
	      "msgid \"\"\n"
	      "msgstr \"\"\n"
	      "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"
	      "\"POT-Creation-Date: 2006-07-01 16:45+0200\\n\"\n"
	      "\"PO-Revision-Date: "++rfc3339()++"\\n\"\n"
	      "\"Last-Translator: "++Name++" <"++Email++">\\n\"\n"
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
take(T, Offset, N, LC, S) -> take(T, Offset - 1, N, LC, S).

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

match_entry({K, _V}, {Str, {key, true}, {value, false}}) ->
    run_re(K, Str);
match_entry({_K, V}, {Str, {key, false}, {value, true}}) ->
    run_re(V, Str);
match_entry({K, V}, {Str, {key, true}, {value, true}}) ->
    case run_re(K, Str) of
	nomatch -> run_re(V, Str);
	_       -> match
    end;
match_entry({_K, _V}, {_Str, {key, false}, {value, false}}) ->
    nomatch.

%% When the translation is empty (as in "") the V is header_info. Weird...
run_re(header_info, _RegExp) ->
    nomatch;
run_re(V, RegExp) ->
    case re:run(V, RegExp) of
	nomatch -> nomatch;
	_       -> match
    end.

run_validators(_F, _Key, _Val, []) ->
    ok;
run_validators(F, Key, Val, [Validator|T]) ->
    case F(Validator, Key, Val) of
	ok                -> run_validators(F, Key, Val, T);
	{error, _Msg} = E -> E
    end.

update_po_files(DefaultPo, [LC|T]) ->
    case read_and_check_po_file(LC) of
	{duplicated, _D} = Duplicated -> 
	    Duplicated;
	LCPo ->
	    wash_po_file(LCPo, DefaultPo, LC),
	    update_po_files(DefaultPo, T)
    end;
update_po_files(_DefaultPo, []) ->
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

wash_po_file(PoToWash, DefaultPo, LC) ->
    PoToWash1 = add_new_delete_old_keys(PoToWash, DefaultPo),
    case PoToWash =:= PoToWash1 of
	false -> 
	    error_logger:info_msg("Updating "++LC++"...~n"),
	    write_po_file(LC, PoToWash1, "Polish tool", "polish@polish.org");
	true  -> ok
    end.
    
add_new_delete_old_keys(PoToWash, DefPo) ->
    add_new_delete_old_keys(PoToWash, DefPo, []).
add_new_delete_old_keys([{K, _V1} = KV|PoToWash], [{K, _V2}|DefPo], Acc) ->
    add_new_delete_old_keys(PoToWash, DefPo, [KV|Acc]);
add_new_delete_old_keys([{K1, _V1}|_]=PoToWash, [{K2, _V2} = KV|DefPo], Acc) when K1 > K2 ->
    add_new_delete_old_keys(PoToWash, DefPo, [KV|Acc]);
add_new_delete_old_keys([{_K, _V}|PoToWash], DefPo, Acc) ->
    add_new_delete_old_keys(PoToWash, DefPo, Acc);
add_new_delete_old_keys([], [KV|DefPo], Acc) ->
    add_new_delete_old_keys([], DefPo, [KV|Acc]);
add_new_delete_old_keys(_, [], Acc) ->
    lists:reverse(Acc).

sort_po_file(LC) ->
    LCPo = read_po_file(LC),
    SortedPo = lists:keysort(1, LCPo),
    case SortedPo =:= LCPo of
	true  -> ok;
	false ->
	    LC1 = case is_atom(LC) of
		      true -> atom_to_list(LC);
		      false -> LC
		  end,
	    error_logger:info_msg(LC1++" po file not sorted. Sorting."),
	    write_po_file(LC, SortedPo, "Polish tool", "polish@polish.org")
    end.

