-module(languages_controller).

-export([dispatch/1, top/1, language/1]).

-include("polish.hrl").

dispatch({_Req, _ResContentType, Path, _Meth} = Args) ->
    F = case Path of
	    ""  -> top;
	    [_] -> language;
	    _   -> erlang:error(bad_uri)
    end,
    apply(?MODULE, F, [Args]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
top({_Req, CT = "application/json", _Path, get}) ->
    try
	Data = [[{lang, LC}, {name, gettext_iso639:lc2lang(LC)}]
		|| LC <- polish:all_custom_lcs()],
	Response = json(Data),
	{?OK, [{?CT, CT}], Response}
    catch
	throw:bad_request->
	    {?BAD_REQUEST, [{?CT, "text/plain"}], ?BAD_REQUEST_MSG}
    end;
top({_Req, _CT, _Path, get}) ->
    {?NOT_SUPPORTED, [{?CT, "text/plain"}], ?NOT_SUPPORTED_MSG};
top({_Req, _ResContentType, _Path, post}) ->
    {?BAD_METHOD, [{?CT, "text/plain"}], ?BAD_METHOD_MSG};
top({_Req, _ResContentType, _Path, delete}) ->
    {?BAD_METHOD, [{?CT, "text/plain"}], ?BAD_METHOD_MSG};
top({_Req, _ResContentType, _Path, put}) ->
    {?BAD_METHOD, [{?CT, "text/plain"}], ?BAD_METHOD_MSG}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /languages/language
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
language({_Req, CT = "application/json", [LC], get}) ->
    try
	{Total, Untrans} = polish_po:get_stats(LC),
	Response = json([{total, Total}, {untrans, Untrans}]),
	{?OK, [{?CT, CT}], Response}
    catch
	throw:bad_request->
	    {?BAD_REQUEST, [{?CT, "text/plain"}], ?BAD_REQUEST_MSG};
	throw:bad_uri ->
	    {404, [{?CT, "text/plain"}], "Not found"}
    end;
language({_Req, _CT, _Path, get}) ->
    {?NOT_SUPPORTED, [{?CT, "text/plain"}], ?NOT_SUPPORTED_MSG};
language({_Req, _ResContentType, _Path, post}) ->
    {?BAD_METHOD, [{?CT, "text/plain"}], ?BAD_METHOD_MSG};
language({_Req, _ResContentType, _Path, delete}) ->
    {?BAD_METHOD, [{?CT, "text/plain"}], ?BAD_METHOD_MSG};
language({_Req, _ResContentType, _Path, put}) ->
    {?BAD_METHOD, [{?CT, "text/plain"}], ?BAD_METHOD_MSG}.

json(Data) ->
    mochijson2:encode({array, Data}).
