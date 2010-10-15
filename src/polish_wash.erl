%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_wash).


-export([write_po_file/4
	 , read_po_file/1
	 , update_po_files/2
	 , sort_po_files/1
	 , get_status_po_files/1
	 , get_new_old_keys/1
        ]).

-import(polish_utils,
        [get_language_name/1
	 , year2str/0
         , rfc3339/0
        ]).

-include("polish.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A P I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_po_file(LC, KVs, Name, Email) ->
    Fname = mk_po_filename(LC),
    Tname = Fname++".tmp",
    Bupname = Fname++"__"++rfc3339(),
    mv(Fname, Bupname),
    {ok,Fd} = file:open(Tname, [write]),
    EditPoHeader = polish_deps:get_env(edit_po_header, false),
    write_header(Fd, LC, Name, Email, EditPoHeader),
    write_entries(Fd, KVs),
    file:close(Fd),
    mv(Tname,Fname),
    ok.

read_po_file(LC) ->
    try case gettext:parse_po(mk_po_filename(LC)) of
	    []    -> [];
	    [_|T] -> T
	end
    catch error:{badmatch, {error, enoent}} -> []
    end.

update_po_files(CustomLCs, KeysToBeReplaced0) ->
    %% sort default po file because it may not be completely sorted even
    %% after a make run_gettext. Strings that contain backslashes have problems.
    KeysToBeReplaced = get_keys_to_be_replaced_from_ids(KeysToBeReplaced0),
    sort_po_file(default),
    DefaultPo = read_po_file(default),
    {NewKeys, _} = get_new_old_keys(DefaultPo, hd(CustomLCs)),
    case update_po_files(DefaultPo, CustomLCs, KeysToBeReplaced) of
	ok    -> polish_utils:print_email_to_translators(NewKeys);
	Dupli -> Dupli
    end.

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

% write_po_file
%------------------------------------------------------------------------------
% NB: We ignore any dynamic info for now, aiming for as fixed header as
% possible in order to avoid annoying PO-file diffs when doing VC-merging.
write_header(Fd, LC, _Name, _Email, _EditPoHeader) ->
    io:format(Fd, gettext:mk_polish_style_header(LC), []).

write_entries(Fd, KVs) ->
    F = fun({header_info,_Val}) ->
		ok;
	   ({Key, Val}) ->
		file:write(Fd, "\nmsgid \"\"\n"),
		gettext:write_pretty(Key, Fd),
		file:write(Fd, "msgstr \"\"\n"),
		gettext:write_pretty(Val, Fd)
	end,
    lists:foreach(F, KVs).


mv(Tname, Fname) ->
    os:cmd("mv "++Tname++" "++Fname).


% update_po_files
%------------------------------------------------------------------------------
update_po_files(DefaultPo, [LC|T], KeysToBeReplaced) ->
    case read_and_check_po_file(LC) of
	{duplicated, D} = Duplicated ->
	    io:format("~n~n~s", [["* "++io_lib:format("~p~n", [E]) || E <- D]]),
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



% sort_po_files
%------------------------------------------------------------------------------
sort_po_file(LC) ->
    LCPo = read_po_file(LC),
    SortedPo = lists:keysort(1, LCPo),
    case SortedPo =:= LCPo of
	true  ->
            error_logger:info_msg(to_list(LC)++" po file already sorted!"),
            ok;
	false ->
	    error_logger:info_msg(to_list(LC)++" po file not sorted. Sorting..."),
	    write_po_file(LC, SortedPo, "Polish tool", "polish@polish.org")
    end.

to_list(LC) when is_list(LC) -> LC;
to_list(LC) when is_atom(LC) -> ?a2l(LC).

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



% get_status_po_files
%------------------------------------------------------------------------------
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



% get_new_old_keys
%------------------------------------------------------------------------------
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
    get_keys_to_be_replaced_from_ids(KeysToBeReplaced, NLK,
				     [{NewKey,OldKey}|Acc]);
get_keys_to_be_replaced_from_ids([], _NLK, Acc) ->
    Acc.



% utils
%------------------------------------------------------------------------------
mk_po_filename(LC) ->
    Dir = polish:po_lang_dir(),
    CustomDefaultDir = get_custom_or_default_dir(LC),
    LangDir = get_lang_dir(LC),
    filename:join([Dir, CustomDefaultDir, LangDir,"gettext.po"]).

get_custom_or_default_dir(default) -> "default";
get_custom_or_default_dir(_LC)     -> "custom".

get_lang_dir(default) -> polish:get_default_lang();
get_lang_dir(LC)      -> LC.

