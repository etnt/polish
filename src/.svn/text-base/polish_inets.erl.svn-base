%% @author Torbjorn Tornkvist tobbe@klarna.com
%% @copyright YYYY Torbjorn Tornkvist.

%% @doc Callbacks for the polish application.

-module(polish_inets).

-export([start_link/0, stop/0, do/1]).

-include("polish.hrl").

%%% @doc This is the routing table.
routes() ->
    [{"/",          polish_web_index}
     , {"/login",     polish_web_login}
     , {"/logout",    polish_web_logout}
     , {"/auth",      polish_web_auth}
     , {"/write",     polish_web_write}
     , {"/ajax",      polish_web_ajax}
     , {"/nitrogen",  static_file}
     , {"/js",        static_file}
     , {"/css",       static_file}
     , {"/images",    static_file}
    ].


start_link() ->
    inets:start(),
    {ok, Pid} =
        inets:start(httpd,
                    [{port, polish_deps:get_env(port, polish:default_port())}
                     ,{server_name,  polish_deps:get_env(hostname, 
                                                         polish:hostname())}
                     ,{server_root, "."}
                     ,{document_root, polish_deps:get_env(doc_root,"./www")}
                     ,{modules, [?MODULE]}
                     ,{mime_types, [{"css", "text/css"},
                                    {"js", "text/javascript"},
                                    {"html", "text/html"}]}
                    ]),
    link(Pid),
    io:format("~p: ~p~n", [?MODULE,Pid]),
    {ok, Pid}.

stop() ->
    httpd:stop_service({any, polish:default_port()}),
    ok.

do(Info) ->
    RequestBridge = simple_bridge:make_request(inets_request_bridge, Info),
    ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    replace_route_handler(),
    nitrogen:run().

replace_route_handler() ->
    wf_handler:set_handler(named_route_handler, routes()).

