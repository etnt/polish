%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_login_controller).

-export([dispatch/1]).

-include("polish.hrl").

dispatch({Req, CT, Path, _Meth}) ->
    case Path of
	[] -> start_openid_authentication(Req, CT);
	_  -> ok
    end.

start_openid_authentication(Req, CT) ->
    try
	ClaimedId = eopenid_lib:http_path_norm(
		      ?lkup(claimed_id, Req:parse_qs())),
	URL = polish_utils:build_url(),
	OpenIdURL = generate_openid_url(ClaimedId, URL),
	{?FOUND, {OpenIdURL, ?HTML++";"++?CHARSET}, ""}
    catch
	_:_ ->
	    {?OK, CT, polish_login_format:login_error("bad_login")}
    end.

generate_openid_url(ClaimedId, URL) ->
    Data0 = eopenid_lib:foldf(
	      [eopenid_lib:in("openid.return_to", URL++"/auth"),
	       eopenid_lib:in("openid.trust_root", URL)
	      ], eopenid_lib:new()),
    {ok, Data1} = eopenid_v1:discover(ClaimedId, Data0),
    {ok, Data2} = eopenid_v1:associate(Data1),
    {ok, OpenIdURL} = eopenid_v1:checkid_setup(Data2),
    OpenIdURL.
