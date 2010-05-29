%%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% @copyright (C) 2010, Torbjorn Tornkvist

-module(polish_web_logout).

-export([main/0]).

-include("polish.hrl").


main() -> 
    case wf:user() of
	undefined -> wf:redirect("/");
	_ ->
	    polish_server:unlock_user_keys(),
	    wf:user(undefined),
	    wf:session(authenticated, false),
	    wf:redirect("/")
    end.


	
