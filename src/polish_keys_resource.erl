%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_resource).

-export([get_list/1, get/1]).

-include("polish.hrl").

get_list(_Query) ->
     [LC || LC <- polish:all_custom_lcs(), LC =/= "a"].

get(Key) ->
    polish_server:read_key(Key).
