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
    "/style.css" ->
      Req:serve_file("style.css", "priv/");
    "/images/" ++ Img ->
      Req:serve_file(Img, "priv/images/");
    "/" ->
      Req:serve_file("index.html", "priv/");
    "/js.js" ->
      Req:serve_file("js.js", "priv/");
    "/jquery-1.4.2.min.js" ->
      Req:serve_file("jquery-1.4.2.min.js", "priv/");
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
  when Controller =/= polish_login_controller ->
  Req:respond({?FOUND, [{"Location", "/login"}], []});
run_controller(Req, Controller, Args, _UserLogged) ->
  case (catch apply(Controller, dispatch, Args)) of
    {'EXIT', Err} ->
      error_logger:format("~p~n", [Err]),
      Req:respond({?INTERNAL_SERVER_ERROR, [{?CT, "text/plain"}],
		   ?INTERNAL_SERVER_ERROR_MSG});
    {Status, Location, ContentType, Cookie, Data} ->
      Headers = [{"Location", Location}, {?CT, ContentType},
		 mochiweb_cookies:cookie(auth, Cookie, [])],
      Req:respond({Status, Headers, Data});
    {Status, ContentType, CookieV, Data} ->
      Headers = [{?CT, ContentType},
		 mochiweb_cookies:cookie(auth, CookieV, [])],
      Req:respond({Status, Headers, Data});
    {Status, ContentType, Data} ->
      {"auth", CookieV} = lists:keyfind("auth", 1, Req:parse_cookie()),
      Headers = [{?CT, ContentType},
		 mochiweb_cookies:cookie(auth, CookieV, [])],
      Req:respond({Status, Headers, Data})
  end.

% Parses the path and returns {top_controller, rest}
parse_path(Path) ->
  CleanedPath = clean_path(Path),
  case string:tokens(CleanedPath, "/") of
    [] ->
      {index, no_path};
    [Top|Rest] ->
      {?l2a("polish_" ++ Top ++ "_controller"), Rest}
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
