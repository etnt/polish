%%% @author Nadia Mohedano-Troyano <nadia@klarna.com>
%%% @copyright (C) 2010, Nadia Mohedano-Troyano
-module(login_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/polish.hrl").

suite() ->
    [].

all() ->
    [start_authentication
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% I N I T S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
    polish_test_lib:start_polish_for_test(),
    Config.

%% init_per_testcase(http_get_key, Config) ->
%%     [{key, "jag heter POlish"}, {translation, "em dic POlish"}|Config];
init_per_testcase(_TestCase, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% E N D S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_per_suite(_Config) ->
    ok.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% T E S T   C A S E S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_authentication(_Config) ->
    ClaimedId = "jordi-chacon.myopenid.com",
    {Code, Headers, _ResponseJSON} = polish_test_lib:send_http_request(
    				       get, [{autoredirect, false}],
				       "/login?claimed_id=" ++ ClaimedId,
    				       ?JSON, headers),
    ?assertEqual(?FOUND, Code),
    Url = polish_utils:url_decode(?lkup("location", Headers)),
    check_identity_in_url(Url, ClaimedId),
    check_assoc_handle_in_url(Url),
    ok.

check_identity_in_url(Url, ClaimedId) ->
    {match, [_, {Start, Length}]} = re:run(Url, "openid.identity=(.*)&",
					   [ungreedy]),
    Identity = lists:sublist(Url, Start+1, Length),
    ?assertEqual({match, [{7, 25}]}, re:run(Identity, ClaimedId)).

check_assoc_handle_in_url(Url) ->
    {match, [_, {Start, Length}]} = re:run(Url, "openid.assoc_handle=(.*)&",
					  [ungreedy]),
    AuthId = lists:sublist(Url, Start+1, Length),
    ?assertMatch([_,_,_,_,_,_],  polish_server:read_user_auth(AuthId)).
