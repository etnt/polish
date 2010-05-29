%% @author Torbjorn Tornkvist tobbe@klarna.com
%% @copyright YYYY Torbjorn Tornkvist.

-module(polish_common).

-include("polish.hrl").

-export([header/1
         , footer/0
         , right/0
         , left/0
        ]).



right() ->
    #panel { class=menu, body=["RIGHT"] }.


left() ->
    #panel { class=menu, body=["LEFT"] }.


header(Selected) ->
    wf:wire(Selected, #add_class { class=selected }),
    #panel { class=menu, body=[
        #link { id=finnish,    url='/?po=fi',        text="Finnish"  },
        #link { id=norwegian,  url='/?po=nb',        text="Norwegian"  },
        #link { id=dannish,    url='/?po=da',        text="Dannish" },
        #link { id=german,     url='/?po=de',        text="German"  },
        #link { id=dutch,      url='/?po=nl',        text="Dutch"  },
	#literal { text = "    -    "},
        #link { class=search,  url='#search_form',     text="Search"  },
        #link { class=statsbutton,   url='#stats',           text="Status"  },
        #link { id=write,  url='/?action=show_changes',text="Generate po file"  },
        #link { id=write,  url='/logout',text="Logout"  }
    ]}.


footer() ->
    [#br{},
     #panel { class=credits, body=[
        ""
%        Copyright &copy; 2010 <a href='http://www.redhoterlang.com'>Torbjorn Tornkvist</a>. 
%        Released under the MIT License.
    ]}].

