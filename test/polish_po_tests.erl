-module(polish_po_tests).

-import(polish_po, [add_new_delete_old_keys/2]).

-include_lib("eunit/include/eunit.hrl").

add_new_delete_old_keys_test_() ->
    PoToWash = [{"a", "aa"}, {"b", "bb"}],
    DefPo = [{"a", "aa"}, {"b", "bb"}],
    PoToWash2 = [{"a", "aa"}, {"b", "bb"}],
    DefPo2 = [{"a", "aa"}, {"b", "bb"}, {"c", "cc"}],
    PoToWash3 = [{"a", "aa"}, {"ab", "lol"}, {"b", "bb"}],
    DefPo3 = [{"a", "aa"}, {"b", "bb"}, {"c", "cc"}],
    PoToWash4 = [{"a", "a"}, {"ab", "ac"}, {"abc", "lol"}, 
		 {"b", "bus"}, {"d", "dd"}],
    DefPo4 = [{"ab", "aa"}, {"b", "bb"}, {"c", "cc"}],
    PoToWash5 = [{"a", "a"}, {"ab", "ac"}, {"abc", "lol"}, 
		 {"b", "bus"}, {"d", "dd"}],
    DefPo5 = [{"7", "77"}, {"ab", "aa"}, {"ac", "acc"}, {"b", "bb"}, 
	      {"c", "cc"}, {"zz", "z"}],
    [?_assertEqual([{"a", "aa"}, {"b", "bb"}], add_new_delete_old_keys(PoToWash, DefPo)),
    ?_assertEqual([{"a", "aa"}, {"b", "bb"}, {"c", "cc"}], 
		  add_new_delete_old_keys(PoToWash2, DefPo2)),
    ?_assertEqual([{"a", "aa"}, {"b", "bb"}, {"c", "cc"}], 
		  add_new_delete_old_keys(PoToWash3, DefPo3)),
    ?_assertEqual([{"ab", "ac"}, {"b", "bus"}, {"c", "cc"}], 
		  add_new_delete_old_keys(PoToWash4, DefPo4)),
    ?_assertEqual([{"7", "77"}, {"ab", "ac"}, {"ac", "acc"}, 
		   {"b", "bus"}, {"c", "cc"}, {"zz", "z"}],
		  add_new_delete_old_keys(PoToWash5, DefPo5))].
