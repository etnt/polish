%% @author Torbjorn Tornkvist tobbe@klarna.com
%% @copyright YYYY Torbjorn Tornkvist.

-module(polish_common).

-include("polish.hrl").

-export([header/1
         , footer/0
         , right/0
         , left/0
        ]).

-import(polish, [all_custom_lcs/0, l2a/1]).


right() ->
    #panel { class=menu, body=["RIGHT"] }.


left() ->
    #panel { class=menu, body=["LEFT"] }.


header(Selected) ->
    #panel { class=menu, body=[
        #list{body=[build_dropdown_menu(Selected),
	#listitem{ body=[#link { text = "    -    "}]},
        #listitem{ body=[#link { class=search, url='#search_form', text="Search"}]},
        #listitem{ body=[#link { class=statsbutton, url='#stats', text="Status"}]},
        #listitem{ body=[#link { id=write, url='/?action=show_changes', text="Submit Translations"}]},
        #listitem{ body=[#link { class=statsbutton, url='#help', text="Help"}]},
        #listitem{ body=[#link { id=write, url='/logout', text="Logout"}]}]}
    ]}.

build_dropdown_menu(Selected0) ->
    Selected = case Selected0 of
		   [] -> "Languages";
		   _  -> Selected0
	       end,
    #listitem{body = [#link{text=Selected}, #list{body = lang_links()}]}.

lang_links() ->
    [#listitem { body = [#link{id=list_to_atom(string:to_lower(gettext_iso639:lc2lang(LC))),    
             url="/?po="++LC,        
             text=gettext_iso639:lc2lang(LC)  }]}
     || LC <- all_custom_lcs()].



footer() ->
    [#br{},
     #panel { class=credits, body=[
        ""
%        Copyright &copy; 2010 <a href='http://www.redhoterlang.com'>Torbjorn Tornkvist</a>. 
%        Released under the MIT License.
    ]}].

