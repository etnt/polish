%% @author Torbjorn Tornkvist tobbe@klarna.com
%%% @copyright (C) 2010, Torbjorn Tornkvist

-module(polish_web_index).

-include("polish.hrl").

-export([main/0,
         title/0,
         layout/0,
	 gen_stats/0,
         inplace_textarea_ok_event/2,
         inplace_textarea_mark_translated_event/1,
	 event/1
	]).

main() ->
     ?AUTH(mainA()).

mainA() ->
    #template { file="./templates/grid.html" }.

title() ->
    "polish".

layout() ->
    {LC, _Action} = Info = get_lang_and_action(),
    Header = polish_common:header(polish_utils:get_language_name(LC)),
    polish_server:unlock_user_keys(),
    #container_12 {
        body=[
              #grid_12 { class = header, body = Header},
              #grid_clear {},
              #grid_12 { body=mk_body(polish_po:get_entries(Info))},
              #grid_clear {},
              #grid_12 { body=polish_common:footer() }
             ]}.

get_lang_and_action() ->
    LC = maybe_reset_session(),
    Action = 
	case wf:qs("action") of
	    ["search"]            -> get_search_request(search);
	    ["save_search"]       -> get_search_request(save_search);
	    ["show_changes"]      -> changes;
	    ["save"]              -> save;
	    ["always_translate"]  -> always_translate;
	    _                     -> po_file
    end,
    {LC, Action}.

get_search_request(Action) ->
    [Str] = wf:qs("search_string"),
    Untrans = get_checkbox_from_qs("untranslated"),
    Trans = get_checkbox_from_qs("translated"),
    Key = get_checkbox_from_qs("key"),
    Value = get_checkbox_from_qs("value"),
    MatchType = get_radiobutton_from_qs(match_type, "match_type"),
    {Action, Str, {{translated, Trans}, {untranslated, Untrans},
		   {key, Key}, {value, Value}, {match_type, MatchType}}}.

get_checkbox_from_qs(K) ->
    case wf:qs(K) of
	[] -> false;
	_  -> true
    end.

get_radiobutton_from_qs(match_type, K) ->
    case wf:qs(K) of
	["match_any_word"] ->
	    any;
	["match_exact_phrase"] ->
	    exact
    end.
    
maybe_reset_session() ->
    case wf:qs("po") of
	[LC] -> maybe_update_lang(LC), LC;
	[]   -> wf:session(offset, 0), wf:session(lang)
    end.

maybe_update_lang(LC) ->
    case wf:session(lang) of
	LC -> ok;
	_  -> wf:session(lang, LC), wf:session(offset, 0)
    end.

%% We don't want those '&nbsp;', thus html_encode=false.
%% However, we do want to escape '<', so some massage is needed.
mk_body([]) ->
    #literal{text="Select a language"};
mk_body({changes, [], _}) ->
    maybe_show_notification(no_changes);
mk_body({save_search, [], _}) ->
    maybe_show_notification(save_search);
mk_body({_Action, [], _}) ->
    maybe_show_notification(no_results);
mk_body({Action, Entries, MoreEntries}) ->
    TableHeader = [#tableheader{text = "Key"}, #tableheader{text = "Translation"}],
    Rows = [build_row(Key, polish_utils:trim_whitespace(Val)) || 
	       {Key,Val} <- lists:reverse(Entries)],
    [maybe_show_notification(Action), #table{rows = TableHeader ++ Rows}, 
     generate_buttons(Action, MoreEntries)].

maybe_show_notification(Action) ->
    Text = case Action of
	       save             -> "Your translation has been submitted.";
	       save_search      -> "Your translation has been submitted.";
	       always_translate -> "Your selection has been marked as always translated.";
	       no_results       -> "No entries found matching the criteria.";
	       bad_search       -> "Your search criteria matched too many texts. "
				       "Please try to make it more restrictive.";
	       _                -> no_text
    end,
    case Text of
	no_text -> [];
	Text    -> #label{text = Text, class="notification"}
    end.

s(K,"", S2)          -> ibox(K,"__empty__", S2);
s(K,header_info, S2) -> ibox(K,"__empty__", S2);
s(K,S, S2)           -> ibox(K,m(S), S2).

ibox(K,S, NotScapedS) ->
    #inplace_textarea { text=S , text_textarea= NotScapedS,
			tag=K , html_encode=false}.

m(header_info) -> "";
m([$<|T]) -> [$&,$l,$t,$;|m(T)];
m([H|T])  -> [H|m(T)];
m([])     -> [].

build_row(Key, Val) ->
    #tablerow { cells = [#tablecell { text = m(Key),
				     class = "msgid",
				     html_encode = false },
			 #tablecell { body = s(Key, Val, Val),
				      html_encode = false,
				      class = "msgval"}]}.

generate_buttons(Action, true) when Action =:= po_file;
				    Action =:= save;
				    Action =:= save_search;
				    Action =:= always_translate;
				    Action =:= search;
				    Action =:= bad_search ->
    Next = [#button{text = "Next", id = "next_button",
		    postback = next_entries}],
    maybe_generate_previous_button() ++ Next;
generate_buttons(_Action, false) ->
    maybe_generate_previous_button().

maybe_generate_previous_button() ->
    case wf:session(offset) of
	undefined     -> [];
	O when O =< 0 -> [];
	O when O > 0  ->
	    [#button{text = "Previous", id = "prev_button",
		     postback =previous_entries}]
    end.

gen_stats() ->
    {LC, _A} = get_lang_and_action(),
    {Total, Untrans} = polish_po:get_stats(LC),
    [ #label{text = "Number of keys: "},
      #value{text = integer_to_list(Total)},
      #br{},
      #label{text = "Untranslated: "},
      #value{text = integer_to_list(Untrans)}].

%% Save translation
inplace_textarea_ok_event(Key, Val0) ->
    {_Lang, Action} = get_lang_and_action(),
    Val = to_latin1(polish_utils:restore_whitespace(Key, polish_utils:trim_whitespace(Val0))),
    case polish_po:check_correctness(Key, Val) of
	ok ->
	    polish_server:unlock_user_keys(),
	    polish_po:write([{Key, Val}]),
	    Redirect = get_redirect_url(Action),
	    wf:redirect(Redirect);
	{error, Msg} ->
	    {error, Msg, Val0}
    end.

%% Mark as always translated
inplace_textarea_mark_translated_event(Key) ->
    LCa = list_to_atom(wf:session(lang)),
    polish_server:mark_as_always_translated(LCa, Key),
    wf:redirect("?action=always_translate").

%% The stupid browser persist to send utf-8 characters...
to_latin1(Str) ->
    case binary_to_list(<< <<C>> || <<C/utf8>> <= list_to_binary(Str) >>) of
        []   -> Str;
        Lstr -> Lstr
    end.

%% called when Next button pressed
event(next_entries) ->
    wf:session(offset, wf:session(offset) + 20),
    wf:redirect("");
event(previous_entries) ->
    wf:session(offset, wf:session(offset) - 20),
    wf:redirect("").

get_redirect_url({Action, Str0, 
		  {{translated, Trans}, {untranslated, Untrans}, {key, Key}, 
		   {value, Value}, {match_type, MatchType}}}) when
      Action =:= search; Action =:= save_search ->
    Str = wf:url_encode(Str0),
    F = fun(true, Arg) -> "&" ++ Arg ++ "=true";
	   (false, _)  -> "" end,
    TransS = F(Trans, "translated"),
    UntransS = F(Untrans, "untranslated"),
    KeyS = F(Key, "key"),
    ValueS = F(Value, "value"),
    MatchTypeS = atom_to_list(MatchType),
    "?action=save_search&search_string=" ++ Str ++ UntransS ++ 
	TransS ++ KeyS ++ ValueS ++ "&match_type=" ++ MatchTypeS;
get_redirect_url(_Action) ->
    "?action=save".
