%%% -*- erlang-indent-level: 2 -*-
%% @author Torbjorn Tornkvist tobbe@klarna.com
%% @copyright 2010 Torbjorn Tornkvist, Jordi Chacon

%% @doc Callbacks for the polish application.

-module(polish_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("polish.hrl").

start(_, _) ->
  eopenid:start(),
  crypto:start(),
  Res = polish_sup:start_link(),
  polish:start(),
  Res.

stop(_) ->
  eopenid:stop(),
  ok.
