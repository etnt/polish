%%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% @copyright (C) 2010, Torbjorn Tornkvist

-module(polish).

-export([setup_user_info/0
         , hostname/0
         , default_port/0
         , gnow/0
         , date/0
         , i2l/1
        ]).

-include("polish.hrl").
-include_lib("nitrogen/include/wf.hrl").
           

default_port() -> 8080.
          
setup_user_info() ->
    User = wf:user(),
    Users = polish_deps:get_env(users, []),
    [L|_] = [L || {U,L} <- Users, U == User],
    wf:session(name,  proplists:get_value(name,L)),
    wf:session(email, proplists:get_value(email,L)).


gnow() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

date() ->
    erlang:date().


hostname() ->
    {ok,Host} = inet:gethostname(),
    Host.

i2l(I) when is_integer(I) -> integer_to_list(I);
i2l(L) when is_list(L)    -> L.
    

