%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_po).

-export([get_stats/1]).

-include("polish.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A P I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_stats(undefined) -> {0, 0};
get_stats(LC) ->
  KVs = polish_server:read_po_file(LC),
  Untrans = get_amount_untranslated_keys(KVs, ?l2a(LC)),
  {length(KVs), Untrans}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  I N T E R N A L   F U N C T I O N S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% get_stats
%------------------------------------------------------------------------------
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
