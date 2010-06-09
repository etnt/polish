%%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% @copyright (C) 2010, Torbjorn Tornkvist

-module(polish).

-export([setup_user_info/0
         , po_lang_dir/0
         , gettext_dir/0
         , meta_filename/1
         , all_custom_lcs/0
	 , update_po_files/0
	 , sort_po_files/0
         , hostname/0
         , default_port/0
         , gnow/0
         , date/0
         , time/0
         , i2l/1
         , l2a/1
        ]).

-import(polish_deps,[get_env/2]).

-include("polish.hrl").
-include_lib("nitrogen/include/wf.hrl").
           

po_lang_dir() -> get_env(po_lang_dir, "/tmp").
gettext_dir() -> po_lang_dir()++"/..".

meta_filename(LC) ->
    polish:po_lang_dir()++"/custom/"++LC++"/gettext.po.meta".

all_custom_lcs() ->
    LCdirs = os:cmd("(cd "++po_lang_dir()++"; ls custom)"),
    string:tokens(LCdirs, "\n").

update_po_files() ->
    CustomLCs = all_custom_lcs(),
    polish_po:update_po_files(CustomLCs).

sort_po_files() ->
    CustomLCs = all_custom_lcs(),
    polish_po:sort_po_files(CustomLCs).

default_port() -> 8080.
          
setup_user_info() ->
    User = wf:user(),
    Users = polish_deps:get_env(users, []),
    [L|_] = [L || {U,L} <- Users, U == User],
    wf:session(name,  proplists:get_value(name,L)),
    wf:session(email, proplists:get_value(email,L)).


gnow() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

local_time() ->
    calendar:gregorian_seconds_to_datetime(gnow()).

time() ->
    element(2, local_time()).

date() ->
    element(1, local_time()).


hostname() ->
    {ok,Host} = inet:gethostname(),
    Host.

i2l(I) when is_integer(I) -> integer_to_list(I);
i2l(L) when is_list(L)    -> L.

l2a(L) when is_list(L) -> list_to_atom(L);
l2a(A) when is_atom(A) -> A.


    

