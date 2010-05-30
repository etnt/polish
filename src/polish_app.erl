%% @author Torbjorn Tornkvist tobbe@klarna.com
%% @copyright YYYY Torbjorn Tornkvist.

%% @doc Callbacks for the polish application.

-module(polish_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("polish.hrl").

start(_, _) ->
    eopenid:start(),
    Res = polish_sup:start_link(),
    load_always_translated_keys(),
    {ok,_Pid} = polish_inets:start_link(), % ends up under the inets supervisors
    Res.
    

stop(_) ->
    eopenid:stop(),
    ok.

load_always_translated_keys() ->
    LCdirs = os:cmd("(cd "++polish:po_lang_dir()++"; ls custom)"),
    case string:tokens(LCdirs, "\n") of
        []  -> ok;
        LCs -> load_always_translated_keys(LCs)
    end.
    
load_always_translated_keys([H|T]) ->
    polish_server:load_always_translated_keys(list_to_atom(H), 
                                              polish:meta_filename(H)),
    load_always_translated_keys(T);
load_always_translated_keys([]) ->
    ok.

