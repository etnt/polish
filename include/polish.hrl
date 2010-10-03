-define(i2l(I), integer_to_list(I)).
-define(l2i(L), list_to_integer(L)).
-define(a2l(A), atom_to_list(A)).
-define(l2a(L), list_to_atom(L)).

-define(SUPPORTED_MEDIA, [ "application/xhtml+xml", "application/xml",
			   "application/json", "text/html"]).
-define(CT, "Content-Type").
-define(CHARSET, "charset=iso-8859-1").
-define(NOT_SUPPORTED, 406).
-define(NOT_SUPPORTED_MSG, "Not acceptable").
-define(NOT_FOUND, 404).
-define(NOT_FOUND_MSG, "Not found").
