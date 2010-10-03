%% @author Torbjorn Tornkvist tobbe@klarna.com
%% @copyright 2010 Torbjorn Tornkvist, Jordi Chacon

%% @doc Callbacks for the polish application.

-module(polish_app).
-behaviour(application).

-export([start/2, stop/1]).

-import(polish, [all_custom_lcs/0, meta_filename/1, load_po_files/0]).

-include("polish.hrl").

start(_, _) ->
    eopenid:start(),
    Res = polish_sup:start_link(),
    load_always_translated_keys(),
    maybe_replace_keys_or_auto_wash(),
    load_po_files(),
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

maybe_replace_keys_or_auto_wash() ->
    case polish_deps:get_env(ask_replace_keys, true) of
	true ->
	    polish:print_new_old_keys();
	false ->
	    case polish:auto_wash() of
		true  -> polish:update_po_files();
		false -> ok
	    end
    end.
