%%% -*- erlang-indent-level: 2 -*-
%%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% @copyright (C) 2010, Torbjorn Tornkvist

-module(polish).

-export([ start/0
	, update_po_files/0
	, update_po_files/1
	, sort_po_files/0
	, get_status_po_files/0
	, compile_templates/0
        ]).

-export([ po_lang_dir/0
        , all_custom_lcs/0
	, get_acl/0
	, get_default_lang/0
	, get_org_name/0
	, get_users/0
        , meta_filename/1
        , hostname/0
	, get_polish_path/0
	, get_env/2
        ]).

-include("polish.hrl").


%%------------------------------------------------------------------------------
%% start functionality
%%------------------------------------------------------------------------------
start() ->
  load_always_translated_keys(),
  maybe_replace_keys_or_auto_wash(),
  load_po_files(),
  compile_templates().

load_always_translated_keys() ->
  load_always_translated_keys(all_custom_lcs()).

load_always_translated_keys([H|T]) ->
  polish_server:load_always_translated_keys(
    list_to_atom(H), meta_filename(H)),
  load_always_translated_keys(T);
load_always_translated_keys([]) ->
  ok.

maybe_replace_keys_or_auto_wash() ->
  case get_env(ask_replace_keys, true) of
    true  -> print_new_old_keys();
    false ->
      case get_env(auto_wash, false) of
	true  -> update_po_files();
	false -> ok
      end
  end.

load_po_files() ->
  CustomLCs = all_custom_lcs(),
  polish_server:load_po_files(CustomLCs).

compile_templates() ->
  Path = get_polish_path() ++ "/",
  Templates = string:tokens(os:cmd("ls " ++Path++ "templates/*_dtl.html"),"\n"),
  [erlydtl:compile(Template, template_name(Template), [{out_dir, "ebin/"}])
   || Template <- Templates].

template_name(Template0) ->
  Template = lists:last(string:tokens(Template0, "/")),
  ?l2a(hd(string:tokens(Template, "."))).

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

sort_po_files() ->
  CustomLCs = all_custom_lcs(),
  polish_wash:sort_po_files(CustomLCs).

get_status_po_files() ->
  CustomLCs = all_custom_lcs(),
  polish_wash:get_status_po_files(CustomLCs).


%%------------------------------------------------------------------------------
%% get various info from the system
%%------------------------------------------------------------------------------

get_env(Key, Default) ->
  case application:get_env(polish, Key) of
    {ok, Value} -> Value;
    _           -> Default
  end.

po_lang_dir() ->
  get_env(po_lang_dir, "/tmp").

all_custom_lcs() ->
  LCdirs = os:cmd("(cd " ++ po_lang_dir() ++ "; ls custom)"),
  string:tokens(LCdirs, "\n").

meta_filename(LC) ->
  po_lang_dir()++"/custom/"++LC++"/gettext.po.meta".

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

hostname() ->
  {ok,Host} = inet:gethostname(),
  Host.

get_polish_path() ->
  PWD0 = lists:takewhile(
	   fun("polish") -> false;
	      (_)        -> true
	   end, string:tokens(os:cmd("pwd"), "/")),
  "/" ++ string:join(PWD0++["polish"], "/").
