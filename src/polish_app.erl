%% @author Torbjorn Tornkvist tobbe@klarna.com
%% @copyright 2010 Torbjorn Tornkvist, Jordi Chacon

%% @doc Callbacks for the polish application.

-module(polish_app).
-behaviour(application).

-export([start/2, stop/1]).

-import(polish, [all_custom_lcs/0, meta_filename/1]).

-include("polish.hrl").

start(_, _) ->
    eopenid:start(),
    Res = polish_sup:start_link(),
    load_always_translated_keys(),
    maybe_auto_wash(),
    {ok,_Pid} = polish_inets:start_link(), % ends up under the inets supervisors
    Res.
    

stop(_) ->
    eopenid:stop(),
    ok.

load_always_translated_keys() ->
    load_always_translated_keys(all_custom_lcs()).
    
load_always_translated_keys([H|T]) ->
    polish_server:load_always_translated_keys(list_to_atom(H), 
                                              meta_filename(H)),
    load_always_translated_keys(T);
load_always_translated_keys([]) ->
    ok.

maybe_auto_wash() ->
    case polish:auto_wash() of
        true  -> polish:update_po_files();
        false -> false
    end.
            
