%%% -*- erlang-indent-level: 2 -*-
%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_login_controller).

-export([dispatch/1]).

-include("polish.hrl").

dispatch({Req, CT, _Path, _Meth}) ->
  case lists:keyfind("action", 1, Req:parse_qs()) of
    false                -> start_openid_authentication(Req, CT);
    {"action", "auth"}   -> finish_openid_authentication(Req, CT);
    {"action", "logout"} -> logout(Req, CT)
  end.

% start authentication
%------------------------------------------------------------------------------
start_openid_authentication(Req, CT) ->
  URL = polish_utils:build_url(),
  ClaimedId = get_claimed_id(Req),
  try
    %% stop prints to shell: when a user enters a wrong formatted openid
    %% username, lots of useless stuff is printed to the shell because
    %% http:request in eopenid fails
    error_logger:tty(false),
    OpenIdData = generate_openid_data(ClaimedId, URL),
    error_logger:tty(true),
    case is_user_allowed(ClaimedId) of
      false ->
	{?OK, CT, polish_login_format:login_error(not_allowed, CT), []};
      true ->
	OpenIdURL = generate_openid_url(OpenIdData),
	write_openid_data(OpenIdData),
	{?FOUND, ?HTML++";"++?CHARSET, [], [{"Location", OpenIdURL}]}
    end
  catch
    _:_ ->
      error_logger:tty(true),
      error_logger:info_msg("Someone tried to login with a wrong formatted "
			    "openid username:~n " ++ ClaimedId ++ "~n~n"),
      {?OK, CT, polish_login_format:login_error(bad_format, CT), []}
  end.

get_claimed_id(Req) ->
  eopenid_lib:http_path_norm(?lkup("claimed_id", Req:parse_qs())).

is_user_allowed(ClaimedId) ->
  lists:member(ClaimedId, polish:get_acl()).

generate_openid_data(ClaimedId, URL) ->
  Data0 = eopenid_lib:foldf(
	    [eopenid_lib:in("openid.return_to", URL++"/login?action=auth"),
	     eopenid_lib:in("openid.trust_root", URL)
	    ], eopenid_lib:new()),
  {ok, Data1} = eopenid_v1:discover(ClaimedId, Data0, mochiweb),
  {ok, Data2} = eopenid_v1:associate(Data1),
  Data2.

generate_openid_url(Data) ->
  {ok, OpenIdURL} = eopenid_v1:checkid_setup(Data),
  OpenIdURL.

write_openid_data(Data) ->
  AuthId = eopenid_lib:out("openid.assoc_handle", Data),
  polish_server:write_user_auth(AuthId, Data).


% finish authentication
%------------------------------------------------------------------------------
finish_openid_authentication(Req, CT) ->
  try
    RawPath = get_raw_path(Req),
    AuthId = get_openid_auth_id(Req),
    SavedData = polish_server:read_user_auth(AuthId),
    polish_server:delete_user_auth(AuthId),
    true = eopenid_v1:verify_signed_keys(RawPath, SavedData),
    NewAuthId = write_user_data(AuthId, SavedData),
    Cookie = mochiweb_cookies:cookie(auth, NewAuthId, []),
    {?FOUND, CT, NewAuthId, [{"Location", "/"}, Cookie]}
  catch
    _:_ ->
      {?OK, CT, polish_login_format:login_error(error, CT), []}
  end.

get_raw_path(Req) ->
  "/login?action=auth" ++ RawPath = Req:get(raw_path),
  RawPath.

get_openid_auth_id(Req) ->
  ?lkup("openid.assoc_handle", Req:parse_qs()).

write_user_data(AuthId0, Data) ->
  AuthId = string:substr([C || C <- AuthId0, C =/= ${, C =/= $}], 1, 21),
  User = ?lkup("openid.claimed_id", Data),
  [{name, Name}, _] = ?lkup(User, polish:get_users()),
  polish_server:write_user_auth(AuthId, Name),
  error_logger:info_msg("User " ++ Name ++ " logged in.~n~n"),
  AuthId.


% logout
%------------------------------------------------------------------------------
logout(Req, _CT) ->
  {"auth", AuthId} = lists:keyfind("auth", 1, Req:parse_cookie()),
  polish_server:delete_user_auth(AuthId),
  Cookie = mochiweb_cookies:cookie(auth, "", []),
  {?FOUND, "text/plain", "ok", [{"Location", "/"}, Cookie]}.
