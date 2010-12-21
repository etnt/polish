%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_dispatcher).

-export([dispatch/1]).
-include("polish.hrl").

dispatch(Req) ->
  Path = Req:get(path),
  case Path of
    "/favicon.ico" ->
      Req:respond({200, [{?CT, "text/html"}], ""});
    "/css/" ++ CSSFile ->
      Req:serve_file(CSSFile, "www/css/");
    "/images/" ++ Img ->
      Req:serve_file(Img, "www/images/");
    "/js/" ++ JSFile ->
      Req:serve_file(JSFile, "www/js/");
    _ ->
      case parse_accept(Req:get(headers)) of
	not_supported ->
	  Req:respond({?NOT_SUPPORTED, [{?CT, "text/plain"}],
		       ?NOT_SUPPORTED_MSG});
	ContentType ->
	  {Controller, ControllerPath} = parse_path(Path),
	  Meth = clean_method(Req:get(method)),
	  Args = {Req, ContentType, ControllerPath, Meth},
	  run_controller(Req, Controller, [Args],
			 polish_utils:is_user_logged(Req))
      end
  end.

% Call the controller action here
run_controller(Req, Controller, _Args, _UserLogged = false)
  when Controller =/= polish_login_controller andalso
       Controller =/= polish_index_controller ->
  Req:respond({?FOUND, [{"Location", "/"}], []});
run_controller(Req, Controller, Args, _UserLogged) ->
  case (catch apply(Controller, dispatch, Args)) of
    {'EXIT', Err} ->
      log_error(Err),
      Req:respond({?INTERNAL_SERVER_ERROR, [{?CT, "text/plain"}],
		   ?INTERNAL_SERVER_ERROR_MSG});
    {Status, ContentType, Data, ExtraHeaders} ->
      Headers = [{?CT, ContentType}] ++ ExtraHeaders,
      Req:respond({Status, Headers, Data});
    {Status, ContentType, Data} ->
      {"auth", CookieV} = lists:keyfind("auth", 1, Req:parse_cookie()),
      Headers = [{?CT, ContentType},
		 mochiweb_cookies:cookie(auth, CookieV, [])],
      Req:respond({Status, Headers, Data});
    {file, {File, Dir, Headers}} ->
      Req:serve_file(File, Dir, Headers)
  end.

% Parses the path and returns {top_controller, rest}
parse_path(Path) ->
  CleanedPath = clean_path(Path),
  case string:tokens(CleanedPath, "/") of
    []         -> {polish_index_controller, no_path};
    [Top|Rest] -> {?l2a("polish_" ++ Top ++ "_controller"), Rest}
  end.

clean_path(Path) ->
  case string:str(Path, "?") of
    0 -> Path;
    N -> string:substr(Path, 1, N - 1)
  end.

clean_method(M) ->
  case M of
    'OPTIONS' -> get;
    _         -> ?l2a(string:to_lower(?a2l(M)))
  end.

parse_accept(Headers) ->
  Accept = mochiweb_headers:get_value('Accept', Headers),
  ClientAcceptedEncs = mochiweb_util:parse_qvalues(Accept),
  Encs = mochiweb_util:pick_accepted_encodings(
	   ClientAcceptedEncs, ?SUPPORTED_MEDIA, "text/plain"),
  case Encs of
    [] -> not_supported;
    _  -> hd(Encs)
  end.

%% Ugly hack: we want to show the error in the common test logs
%% when a crash has happened while running a testcase. But we want
%% to use normal logging when we are not testing... so this ugly
%% hacks achieves that :)
log_error(Err) ->
  case whereis(ct_util_server) of
    undefined -> error_logger:format("~p~n", [Err]);
    _Pid      -> ct:log("~p~n", [Err])
  end.
