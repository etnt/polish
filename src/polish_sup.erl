%%% -*- erlang-indent-level: 2 -*-
%%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% @copyright (C) 2010, Torbjorn Tornkvist

-module(polish_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

server(Name, Type) ->
  server(Name, Type, 2000).

server(Name, Type, Shutdown) ->
  {Name, {Name, start_link, []}, permanent, Shutdown, Type, [Name]}.

worker(Name) -> server(Name, worker).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
  {ok, {_, Specs}} = init([]),

  Old = sets:from_list(
	  [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  sets:fold(fun (Id, ok) ->
		supervisor:terminate_child(?MODULE, Id),
		supervisor:delete_child(?MODULE, Id),
		ok
	    end, ok, Kill),

  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  PolishServer = worker(polish_server),
  CronServer = worker(polish_cron_server),
  Elogger = worker(elogger),
  GettextServer = {gettext_server,{gettext_server,start_link,[polish]},
		   permanent,5000,worker,[gettext_server]},
  Mochiweb = {mochiweb_server,
	      {mochiweb_http, start,
	       [[{ip, polish:get_env(hostname, "127.0.0.1")},
		 {port, polish:get_env(port, 8080)},
		 {loop, fun polish_dispatcher:dispatch/1}]]},
	      permanent, 5000, worker, [mochiweb_http]},
  {ok, {{one_for_one, 10, 10},
	[PolishServer, GettextServer, CronServer, Elogger, Mochiweb]}}.

