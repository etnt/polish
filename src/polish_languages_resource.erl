%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_languages_resource).

-export([get_list/0, get/1]).

-include("polish.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A P I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_list() ->
  [LC || LC <- polish:all_custom_lcs(), LC =/= "a"].

get(LC) ->
  case get_language_stats(LC) of
    {0, 0} -> throw(bad_uri);
    Stats  -> Stats
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  I N T E R N A L   F U N C T I O N S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_language_stats(undefined) -> {0, 0};
get_language_stats(LC) ->
  KVs = polish_server:read_po_file(LC),
  Untrans = get_amount_untranslated_keys(KVs, ?l2a(LC)),
  {length(KVs), Untrans}.

get_amount_untranslated_keys(KVs, LCa) ->
  lists:foldl(
    fun({K, K}, Acc) ->
	case polish_server:is_always_translated(
	       polish_utils:generate_key_identifier(K, ?a2l(LCa))) of
	  true  -> Acc;
	  false -> Acc + 1
	end;
       ({_K, _V}, Acc) -> Acc
    end, 0, KVs).
