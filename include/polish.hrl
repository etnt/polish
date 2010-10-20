-ifndef(_POLISH_HRL).
-define(_POLISH_HRL, true).

-include_lib("nitrogen/include/wf.hrl").


-define(AUTH(FunCall), 
        case {wf:session(authenticated),
              lists:member(wf:user(), polish:get_acl())} of
            {true,true} -> polish:setup_user_info(), FunCall;
            _           -> wf:redirect("/login")
        end).

-define(i2l(I), integer_to_list(I)).
-define(l2i(L), list_to_integer(L)).
-define(a2l(A), atom_to_list(A)).
-define(l2a(L), list_to_atom(L)).

-endif.
