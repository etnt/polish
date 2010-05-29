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
    PoDir = polish_deps:get_env(po_lang_dir, ""),
    gettext:change_gettext_dir(PoDir),
    load_always_translated_keys([nl, de, da, nb, fi]),
    {ok,_Pid} = polish_inets:start_link(), % ends up under the inets supervisors
    Res.
    

stop(_) ->
    eopenid:stop(),
    ok.

load_always_translated_keys([H|T]) ->
    HStr = atom_to_list(H),
    polish_server:load_always_translated_keys(
      H, "po/gettext.po."++HStr++".meta"),
    load_always_translated_keys(T);
load_always_translated_keys([]) ->
    ok.
