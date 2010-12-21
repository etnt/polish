%%% -*- erlang-indent-level: 2 -*-
%%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% @copyright (C) 2010, Torbjorn Tornkvist

-module(polish).

-export([po_lang_dir/0
         , auto_wash/0
	 , print_new_old_keys/0
	 , get_acl/0
	 , get_default_lang/0
	 , get_org_name/0
	 , get_users/0
         , gettext_dir/0
         , meta_filename/1
         , all_custom_lcs/0
	 , update_po_files/0
	 , update_po_files/1
	 , load_po_files/0
	 , sort_po_files/0
	 , get_status_po_files/0
         , hostname/0
         , default_port/0
         , gnow/0
         , date/0
         , time/0
         , i2l/1
         , l2a/1
         , a2l/1
	 , compile_templates/0
	 , get_polish_path/0
        ]).

-import(polish_deps,[get_env/2]).

-include("polish.hrl").

auto_wash() -> get_env(auto_wash, false).

po_lang_dir() -> get_env(po_lang_dir, "/tmp").
gettext_dir() -> po_lang_dir()++"/..".

meta_filename(LC) ->
  polish:po_lang_dir()++"/custom/"++LC++"/gettext.po.meta".

all_custom_lcs() ->
  LCdirs = os:cmd("(cd "++po_lang_dir()++"; ls custom)"),
  string:tokens(LCdirs, "\n").

print_new_old_keys() ->
  [LC|_] = all_custom_lcs(),
  case polish_wash:get_new_old_keys(LC) of
    {[], []} -> update_po_files();
    {[], _}  -> update_po_files();
    {_, []}  -> update_po_files();
    Keys     -> polish_utils:print_new_old_keys(Keys)
  end.

update_po_files() ->
  update_po_files([]).

update_po_files(KeysToBeReplaced) ->
  CustomLCs = all_custom_lcs(),
  polish_wash:update_po_files(CustomLCs, KeysToBeReplaced).

load_po_files() ->
  CustomLCs = all_custom_lcs(),
  polish_server:load_po_files(CustomLCs).

sort_po_files() ->
  CustomLCs = all_custom_lcs(),
  polish_wash:sort_po_files(CustomLCs).

get_status_po_files() ->
  CustomLCs = all_custom_lcs(),
  polish_wash:get_status_po_files(CustomLCs).

default_port() -> 8080.

get_users() ->
  get_from_meta_file(users).

get_acl() ->
  get_from_meta_file(acl).

get_default_lang() ->
  get_from_meta_file(default_lang).

get_org_name() ->
  get_from_meta_file(org_name).

get_from_meta_file(Field) ->
  PoDir = get_env(po_lang_dir, "/tmp"),
  case file:consult(PoDir ++ "/polish.meta") of
    {ok, List} ->
      case proplists:get_value(Field, List) of
	undefined -> [];
	Data      -> Data
      end;
    _ ->
      []
  end.

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

a2l(A) when is_atom(A) -> atom_to_list(A);
a2l(L) when is_list(L) -> L.

compile_templates() ->
  Path = get_polish_path() ++ "/",
  Templates = string:tokens(os:cmd("ls " ++Path++ "templates/*_dtl.html"),"\n"),
  [erlydtl:compile(Template, template_name(Template), [{out_dir, "ebin/"}])
   || Template <- Templates].

template_name(Template0) ->
  Template = lists:last(string:tokens(Template0, "/")),
  ?l2a(hd(string:tokens(Template, "."))).

get_polish_path() ->
  PWD0 = lists:takewhile(
	   fun("polish") -> false;
	      (_)        -> true
	   end, string:tokens(os:cmd("pwd"), "/")),
  "/" ++ string:join(PWD0++["polish"], "/").
