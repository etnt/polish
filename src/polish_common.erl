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
    wf:wire(l2a(Selected), #add_class { class=selected }),
    #panel { class=menu, body=lang_links()++[
	#literal { text = "    -    "},
        #link { class=search,  url='#search_form',     text="Search"  },
        #link { class=statsbutton,   url='#stats',           text="Status"  },
        #link { id=write,  url='/?action=show_changes',text="Generate po file"  },
        #link { id=write,  url='/logout',text="Logout"  }
    ]}.

lang_links() ->
    [#link { id=list_to_atom(string:to_lower(gettext_iso639:lc2lang(LC))),    
             url="/?po="++LC,        
             text=gettext_iso639:lc2lang(LC)  }
     || LC <- all_custom_lcs()].



footer() ->
    [#br{},
     #panel { class=credits, body=[
        ""
%        Copyright &copy; 2010 <a href='http://www.redhoterlang.com'>Torbjorn Tornkvist</a>. 
%        Released under the MIT License.
    ]}].

