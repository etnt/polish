%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_controller).

-export([dispatch/1, top/1, key/1]).

-include("polish.hrl").

dispatch({_Req, _ResContentType, Path, _Meth} = Args) ->
    F = case Path of
	    ""  -> top;
	    [_] -> key;
	    _   -> erlang:error(bad_uri)
    end,
    apply(?MODULE, F, [Args]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /keys
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
top({Req, CT, _Path, get}) ->
    try
	Query = Req:parse_qs(),
	Data = polish_keys_resource:get_list(Query),
	Response = polish_keys_format:list(Data, CT),
	{?OK, CT, Response}
    catch
	throw:bad_request->
	    {?BAD_REQUEST, "text/plain", ?BAD_REQUEST_MSG};
	throw:not_supported ->
	    {?NOT_SUPPORTED, "text/plain", ?NOT_SUPPORTED_MSG}
    end;
top({_Req, _ResContentType, _Path, post}) ->
    {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
top({_Req, _ResContentType, _Path, delete}) ->
    {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
top({_Req, _ResContentType, _Path, put}) ->
    {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /keys/key
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
key({_Req, CT, [Key], get}) ->
    try
	Data = polish_keys_resource:get(Key),
	Response = polish_keys_format:key(Data, Key, CT),
	{?OK, CT, Response}
    catch
	throw:bad_request->
	    {?BAD_REQUEST, "text/plain", ?BAD_REQUEST_MSG};
	throw:bad_uri ->
	    {?NOT_FOUND, "text/plain", ?NOT_FOUND_MSG};
	throw:not_supported ->
	    {?NOT_SUPPORTED, "text/plain", ?NOT_SUPPORTED_MSG}
    end;
key({_Req, _ResContentType, _Path, post}) ->
    {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
key({_Req, _ResContentType, _Path, delete}) ->
    {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
key({_Req, _ResContentType, _Path, put}) ->
    {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG}.
