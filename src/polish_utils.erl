%%% @author Jordi Chacon <jordi.chacon@klarna.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(polish_utils).

-export([get_language_name/1
	 , get_country_name/1
         , year2str/0
         , translator_name/0
         , translator_email/0
         , rfc3339/0
        ]).


get_language_name(undefined) -> "";
get_language_name(LC)        -> gettext_iso639:lc2lang(LC).

get_country_name("nl") -> "Netherlands";
get_country_name("nb") -> "Norway";
get_country_name("da") -> "Denmark";
get_country_name("de") -> "Germany";
get_country_name("fi") -> "Finland";
get_country_name(undefined) -> "".


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

