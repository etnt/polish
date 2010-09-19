%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_utils).

-include_lib("eunit/include/eunit.hrl").

-export([get_language_name/1
         , year2str/0
         , translator_name/0
         , translator_email/0
         , rfc3339/0
	 , trim_whitespace/1
	 , split_whitespace/1
	 , restore_whitespace/2
	 , print_email_to_translators/1
	 , print_new_old_keys/1
        ]).


get_language_name(undefined) -> "";
get_language_name(LC)        -> gettext_iso639:lc2lang(LC).

year2str() ->
    {Y,_,_} = date(),
    integer_to_list(Y).


translator_name() ->
    wf:session(name).

translator_email() ->
    wf:session(email).


rfc3339() ->
    rfc3339(calendar:now_to_local_time(now())).

rfc3339(Gsec) when is_integer(Gsec) ->
    rfc3339(calendar:gregorian_seconds_to_datetime(Gsec));
rfc3339({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w~s",
                  [Year,Month,Day, Hour, Min, Sec, zone()]).  

zone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
        calendar:datetime_to_gregorian_seconds(Time),
    zone(DiffSecs div 3600, (DiffSecs rem 3600) div 60).

zone(Hr, Min) when Hr < 0; Min < 0 ->
    io_lib:format("-~2..0w~2..0w", [abs(Hr), abs(Min)]);
zone(Hr, Min) when Hr >= 0, Min >= 0 ->
    io_lib:format("+~2..0w~2..0w", [Hr, Min]).

print_email_to_translators([]) -> ok;
print_email_to_translators(NewKeys) ->
    Hostname = polish_deps:get_env(hostname, "localhost"),
    Port = integer_to_list(polish_deps:get_env(port, 8000)),
    URL = "http://" ++ Hostname ++ ":" ++ Port,
    KeysText = lists:foldl(fun(K, Acc) -> "* " ++ K ++ "~n" ++ Acc end, [], NewKeys),
    io:format("~n~n~n~n~nEMAIL TO TRANSLATORS~n"
	      "--------------------~n~n~n"
	      "Hi all,~n~n"
	      "URL:~n" ++ URL ++ "~n~n"
	      "Texts:~n" ++ KeysText ++ "~n"
	      "Ticket information:~n{Paste here ticket specs}~n~n~n"
	      "Best regards,~n~n~n"
	      "--------------------~n~n~n~n~n").    

print_new_old_keys({New, Old}) ->
    io:format("~n~n~n~n~n"),
    io:format("List of new keys and to be removed keys. If you want a new key to"
	      " keep the translations of a key to be removed you need to say so "
	      "when updating the po files. For example:~n"
	      "polish:update_po_files([{1,3},{2,1}]).~n"
	      "will make new key 1 keep translations of old key 3 and new key 2 "
	      "keep translations of old key 1. Otherwise just run "
	      "polish:update_po_files().~n"),
    io:format("--------------------~n~nNEW KEYS~n~n"),
    print_keys(New),
    io:format("~n~nTO BE REMOVED KEYS~n~n"),
    print_keys(Old),
    io:format("~n--------------------~n~n~n~n~n").

print_keys(Keys) ->
    print_keys(Keys, 1).
print_keys([K|Keys], N) ->
    io:format("~p. ~s~n", [N, K]),
    print_keys(Keys, N + 1);
print_keys([], _N) ->
    ok.			       

%% @spec trim_whitespace(Input::string()) -> Result
%%   Result = string()
%% @doc Trims whitespace at start and end of a string.
trim_whitespace(Input) ->
    {_Leading, Txt, _Trailing} = split_whitespace(Input),
    Txt.

%% @spec split_whitespace(Input::string()) -> Result
%%   Result = {Leading, Text, Trailing}
%%   Leading, Text, Trailing = string()
%% @doc Trims whitespace at start and end of a string.
split_whitespace(Input) ->
   {match, [_, Leading, Text, Trailing]} = 
    re:run(Input, "^([\\s]*)(.*?)([\\s]*)$", [{capture, all, list}, dotall]),
   {Leading, Text, Trailing}.
   
trim_whitespace_test()->
    ?assertEqual({"   \t\n\r  ","hej\ng","  "}, trim_whitespace("   \t\n\r  hej\ng  ")). 

%% Restore a trimmed string's original leading and trailing whitespace
%% from the key
restore_whitespace(Orig, Trimmed) ->
    {Leading, _, Trailing} = 
	split_whitespace(Orig),
    lists:append(Leading, lists:append(Trimmed, Trailing)).    

restore_whitespace_test() ->
    Orig = "  \ngreat\t  ",
    {_, Text, _} = trim_whitespace(Orig),
    ?assertEqual(Orig, restore_whitespace(Orig, Text)).
