%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_languages_controller).

-export([dispatch/1, top/1, language/1]).

-include("polish.hrl").

dispatch({_Req, _CT, Path, _Meth} = Args) ->
  F = case Path of
	""  -> top;
	[_] -> language;
	_   -> erlang:error(bad_uri)
      end,
  apply(?MODULE, F, [Args]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /languages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
top({_Req, CT, _Path, get}) ->
  try
    Data = polish_languages_resource:get_list(),
    Response = polish_languages_format:list(Data, CT),
    {?OK, CT, Response}
  catch
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
%% /languages/language
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
language({_Req, CT, [LC], get}) ->
  try
    Data = polish_languages_resource:get(LC),
    Response = polish_languages_format:language(Data, CT),
    {?OK, CT, Response}
  catch
    throw:bad_request->
      {?BAD_REQUEST, "text/plain", ?BAD_REQUEST_MSG};
    throw:bad_uri ->
      {?NOT_FOUND, "text/plain", ?NOT_FOUND_MSG};
    throw:not_supported ->
      {?NOT_SUPPORTED, "text/plain", ?NOT_SUPPORTED_MSG}
  end;
language({_Req, _CT, _Path, post}) ->
  {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
language({_Req, _CT, _Path, delete}) ->
  {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG};
language({_Req, _CT, _Path, put}) ->
  {?BAD_METHOD, "text/plain", ?BAD_METHOD_MSG}.

