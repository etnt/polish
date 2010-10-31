%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_languages_format).

-export([list/2, language/2]).

-include("polish.hrl").


list(Data0, ?JSON) ->
    Data = [{struct, [{url, country_url(LC)}, {name, country_name(LC)}]}
	    || LC <-Data0],
    mochijson2:encode({array, Data});
list(_Data, _CT) ->
    throw(not_supported).

country_name(LC) ->
    ?l2a(polish_utils:to_utf8(gettext_iso639:lc2lang(LC))).

country_url(LC) ->
    ?l2a(polish_utils:build_url() ++ "/languages/" ++ LC).

language({Total, Untrans}, ?JSON) ->
    mochijson2:encode({struct, [{total, Total}, {untrans, Untrans}]});
language(_Data, _CT) ->
    throw(not_supported).
