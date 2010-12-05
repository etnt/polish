%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_keys_controller).

-export([dispatch/1, top/1, key/1]).

-include("polish.hrl").

dispatch({_Req, _CT, Path, _Meth} = Args) ->
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
top({_Req, _CT, _Path, post}) ->
  {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
top({_Req, _CT, _Path, delete}) ->
  {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
top({_Req, _CT, _Path, put}) ->
  {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /keys/keyID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
key({Req, CT, [ID], get}) ->
  User = polish_utils:get_user_from_cookies(Req:parse_cookie()),
  try
    Data = polish_keys_resource:get(ID, User),
    Response = polish_keys_format:key(Data, ID, CT),
    {?OK, CT, Response}
  catch
    throw:bad_request->
      {?BAD_REQUEST, "text/plain", ?BAD_REQUEST_MSG};
    throw:bad_uri ->
      {?NOT_FOUND, "text/plain", ?NOT_FOUND_MSG};
    throw:not_supported ->
      {?NOT_SUPPORTED, "text/plain", ?NOT_SUPPORTED_MSG}
  end;
key({_Req, _CT, _Path, post}) ->
  {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
key({_Req, _CT, _Path, delete}) ->
  {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
key({Req, CT, [ID], put}) ->
  Body = Req:parse_post(),
  User = polish_utils:get_user_from_cookies(Req:parse_cookie()),
  try
    Result = polish_keys_resource:put(ID, Body, User),
    Response = polish_keys_format:put(Result, CT),
    {?OK, CT, Response}
  catch
    throw:bad_request->
      {?BAD_REQUEST, "text/plain", ?BAD_REQUEST_MSG};
    throw:bad_uri ->
      {?NOT_FOUND, "text/plain", ?NOT_FOUND_MSG}
  end.
