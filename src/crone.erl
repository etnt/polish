%%% BEGIN crone.erl %%%
%%%
%%% crone - Task Scheduler for Erlang/OTP
%%% Copyright (c)2002 Cat's Eye Technologies.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc Simple task scheduler for Erlang/OTP.  This application aims to
%% one day be a viable replacement for <code>cron</code>.
%%
%% <p><code>crone</code> is a simple utility which schedules tasks to be
%% run periodically at given times.</p>
%%
%% <p><code>crone</code> differs from <code>cron</code> in the following
%% ways:</p>
%%
%% <ul>
%% <li><code>crone</code> does not support multiple users.
%% Of course, individual users may run their own copies of <code>crone</code>
%% as desired.</li>
%%
%% <li>No configuration files are used.  <code>crone</code> is simply
%% started with a list of task descriptions.  These could easily be stored
%% in a file to be read with <code>file:consult/1</code> if desired.</li>
%%
%% <li><code>crone</code> launches, not system executables, but Erlang
%% functions.  Of course, system programs can be launched with the Erlang
%% function <code>os:cmd/1</code>.</li>
%%
%% <li>The synax of a task description is quite different from
%% <code>crontab</code>.  It is (in this author's opinion) easier to read
%% and is much more in keeping with the Erlang tradition.  It is not
%% quite as expressive as <code>cron</code> but this can be compensated
%% for by adding multiple tasks.</li>
%%
%% <li>No output is logged or mailed to anyone.  If you want output to
%% be logged or mailed, you must explicitly specify that as part of the
%% task.</li>
%%
%% <li><code>crone</code> does not poll the system on a minute-by-minute
%% basis like <code>cron</code> does.  Each task is assigned to a single
%% (Erlang) process.  The time until it is to run next is calculated,
%% and the process sleeps for exactly that long.</li>
%%
%% <li>Unlike <code>cron</code>'s one-minute resolution, <code>crone</code>
%% has a 2-second resolution (actually 1 second, but after running the
%% task, the process waits an extra second to avoid accidentally running it
%% more than once.)</li>
%%
%% <li>Because it does not wake up every minute, and because it does not
%% have a fixed configuration file, <code>crone</code> must be stopped and
%% restarted if the user wishes to change the scheduled tasks.</li>
%%
%% <li><code>crone</code> does not handle Daylight Savings Time (or other
%% cases when the system clock is altered) gracefully, and it is recommended
%% that it be stopped and restarted on such occasions.</li>
%% </ul>
%%
%% @type  task()       = {when(), mfa()}
%% @type  mfa()        = {module(), function(), args()}
%% @type  when()       = {daily, period()}
%%                     | {weekly, dow(), period()} 
%%                     | {monthly, dom(), period()}
%% @type  dow()        = mon | tue | wed | thu | fri | sat | sun
%% @type  dom()        = integer()
%% @type  period()     = time() | [time()] | {every, duration(), constraint()}
%% @type  duration()   = {integer(), hr | min | sec}
%% @type  constraint() = {between, time(), time()}
%% @type  time()       = {integer(), am | pm} | {integer(), integer(), am | pm}
%%
%% @end

-module(crone).
-vsn('2002.0704').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([start/1, stop/1]).
-export([loop_task0/1, loop_task/2, loop_task/3]).
-export([format_time/1, format_task/1]).

%% @spec start([task()]) -> [pid()]
%% @doc Starts <code>crone</code>, spawning a process for each task.
%% XXX: This function is not tested.
start(Tasks) ->
    [spawn(?MODULE, loop_task0, [X]) || X <- Tasks].

%% @spec stop([task()]) -> ok
%% @doc Stops all monitoring processes started by <code>crone</code>.

stop(Tasks) ->
  lists:foreach(fun(X) ->
                  exit(X, stopped)
                end, Tasks),
  ok.

%% @spec loop_task0(task()) -> never_returns
%% @doc Used by <code>start/1</code> to wait until the next time
%% each task is scheduled to run, run it, and repeat.

loop_task0(Task) ->
    process_flag(trap_exit, true),
    loop_task(Task, undefined).

loop_task(Task, Job) ->
    Duration = until_next_time(Task),
    loop_task(Duration, Task, Job).

loop_task(Duration, Task, Job) ->
    %io:fwrite("~p: sleeping for ~s~n", [Task, format_time(Duration)]),
    case isleep(Duration, Task, Job) of
	undefined ->
	    Job2 = run_task(Task),
	    timer:sleep(1000),
	    ?MODULE:loop_task(Task, Job2);
	{remain, Remain, Job2} ->
	    ?MODULE:loop_task(Remain, Task, Job2);
	PID ->
	    error_logger:format("Warning, crontab jobs would overlap "
				" NOT running ~p because ~p~n",
				[Task, {PID, process_info(PID)}]),
	    
	    timer:sleep(1000),
	    ?MODULE:loop_task(Task, PID)
    end.



%% Interruptable sleeper
isleep(Secs, Task, Job) when Secs > 0 ->
    N = polish_utils:gnow(),
    do_isleep(N, Secs, Task, Job);
isleep(_Secs, _Task, Job) ->
    Job.

do_isleep(N, Secs, Task, Job) ->
    receive
	{_From, timechange} ->
	    Later = polish_utils:gnow(),
	    Remain = Secs - (Later - N),
	    if Remain =< 0 ->
		    Job;
	       true ->
		    {remain, Remain, Job}
	    end;
	{From, remain} ->
	    Later = polish_utils:gnow(),
	    Remain = Secs - (Later - N),
	    From ! {self(), Remain, Task},
	    {remain, Remain, Job};
	{_From, {steptime, StepSecs}} ->
	    Later = polish_utils:gnow(),
	    Remain = Secs - (Later - N),
	    {remain, Remain - StepSecs, Job};
	{'EXIT', Job, _} ->
	    Later = polish_utils:gnow(),
	    Remain = Secs - (Later - N),
	    {remain, Remain, undefined};
	{_From, trigger} ->
	    Job
    after Secs * 1000 ->
	    Job
    end.


%% @spec current_time() -> seconds()
%%         seconds() = integer()
%% @doc Returns the current time, in seconds past midnight.

current_time() ->
  {H,M,S} = polish_utils:time(),
  S + M * 60 + H * 3600.

%% @spec until_next_time(task()) -> seconds()
%% @doc Calculates the duration in seconds until the next time
%% a task is to be run.

until_next_time(Task) ->
  {When, _MFA} = Task,
  case When of
    {daily, Period} ->
      until_next_daytime(Period);
    {weekly, DoW, Period} ->
      OnDay = resolve_dow(DoW),
      Today = calendar:day_of_the_week(polish_utils:date()),
      case Today of
        OnDay ->
	  until_next_daytime_or_days_from_now(Period, 7);
        Today when Today < OnDay ->
	  until_days_from_now(Period, OnDay - Today);
	Today when Today > OnDay  ->
	  until_days_from_now(Period, (OnDay+7) - Today)
      end;
    {monthly, DoM, Period} ->
      {ThisYear, ThisMonth, Today} = polish_utils:date(),
      {NextYear, NextMonth} = case ThisMonth of
        12 -> {ThisYear + 1, 1};
	_  -> {ThisYear, ThisMonth + 1}
      end,
      D1 = calendar:date_to_gregorian_days(ThisYear, ThisMonth, Today),
      D2 = calendar:date_to_gregorian_days(NextYear, NextMonth, DoM),
      Days = D2 - D1,
      case Today of
        DoM ->
	  until_next_daytime_or_days_from_now(Period, Days);
	_ ->
	  until_days_from_now(Period, Days)
      end
  end.

%% @spec until_next_daytime(period()) -> seconds()
%% @doc Calculates the duration in seconds until the next time this
%% period is to occur during the day.

until_next_daytime(Period) ->
  StartTime = first_time(Period),
  EndTime = last_time(Period),
  case current_time() of
    T when T > EndTime ->
      until_tomorrow(StartTime);
    T ->
      next_time(Period, T) - T
  end.

%% @spec until_tomorrow(seconds()) -> seconds()
%% @doc Calculates the duration in seconds until the given time occurs
%% tomorrow.

until_tomorrow(StartTime) ->
  (StartTime + 24*3600) - current_time().

%% @spec until_days_from_now(period(), integer()) -> seconds()
%% @doc Calculates the duration in seconds until the given period
%% occurs several days from now.

until_days_from_now(Period, Days) ->
  Days * 24 * 3600 + until_next_daytime(Period).

%% @spec until_next_daytime_or_days_from_now(period(), integer()) -> seconds()
%% @doc Calculates the duration in seconds until the given period
%% occurs, which may be today or several days from now.

until_next_daytime_or_days_from_now(Period, Days) ->
  CurrentTime = current_time(),
  case last_time(Period) of
    T when T < CurrentTime ->
      until_days_from_now(Period, Days-1);
    _T ->
      until_next_daytime(Period)
  end.

%% @spec last_time(period()) -> seconds()
%% @doc Calculates the last time in a given period.

last_time(Period) ->
  hd(lists:reverse(lists:sort(resolve_period(Period)))).

%% @spec first_time(period()) -> seconds()
%% @doc Calculates the first time in a given period.

first_time(Period) ->
  hd(lists:sort(resolve_period(Period))).

%% @spec next_time(period(), seconds()) -> seconds()
%% @doc Calculates the first time in the given period after the given time.

next_time(Period, Time) ->
  R = lists:sort(resolve_period(Period)),
  lists:foldl(fun(X, A) ->
                case X of
		  T when T >= Time, T < A -> T;
		  _ -> A
		end
              end, 24*3600, R).

%% @spec resolve_period(period()) -> [seconds()]
%% @doc Returns a list of times given a periodic specification.

resolve_period([]) -> [];
resolve_period([H | T]) -> resolve_period(H) ++ resolve_period(T);
resolve_period({every, Duration, {between, TimeA, TimeB}}) ->
  Period = resolve_dur(Duration),
  StartTime = resolve_time(TimeA),
  EndTime = resolve_time(TimeB),
  resolve_period0(Period, StartTime, EndTime, []);
resolve_period(Time) ->
  [resolve_time(Time)].

resolve_period0(_Period, Time, EndTime, Acc) when Time >= EndTime -> Acc;
resolve_period0(Period, Time, EndTime, Acc) ->
  resolve_period0(Period, Time + Period, EndTime, [Time | Acc]).

%% @spec resolve_time(time()) -> seconds()
%% @doc Returns seconds past midnight for a given time.

resolve_time({H, M, S, X}) -> resolve_time({H, X}) + M * 60 + S;
resolve_time({H, M, X}) when is_atom(X) -> resolve_time({H, X}) + M * 60;
resolve_time({H, M, S}) -> H * 3600 + M * 60 + S;
resolve_time({12, am}) -> 0;
resolve_time({H,  am}) -> H * 3600;
resolve_time({12, pm}) -> 12 * 3600;
resolve_time({H,  pm}) -> (H + 12) * 3600;
resolve_time({H, M}) -> H * 3600 + M * 60.

%% @spec resolve_dur(duration()) -> seconds()
%% @doc Returns seconds for a given duration.

resolve_dur({Hour, hr}) -> Hour * 3600;
resolve_dur({Min, min}) -> Min * 60;
resolve_dur({Sec, sec}) -> Sec.

%% @spec resolve_dow(dow()) -> integer()
%% @doc Returns the number of the given day of the week. See the calendar
%% module for day numbers.

resolve_dow(mon) -> 1;
resolve_dow(tue) -> 2;
resolve_dow(wed) -> 3;
resolve_dow(thu) -> 4;
resolve_dow(fri) -> 5;
resolve_dow(sat) -> 6;
resolve_dow(sun) -> 7.

%% @spec run_task(task()) -> pid()
%% @doc Spawns a process to accomplish the given task.

run_task(Task) ->
    {_When, MFA} = Task,
    {M,F,A} = MFA,
    proc_lib:spawn_link(M,F,A).


%% @spec format_time(integer()) -> string()
%% @doc Returns human-readable string from seconds past midnight.

format_time(N) ->
  S = N rem 60,
  M = (N div 60) rem 60,
  H = (N div 3600),
  io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [H,M,S]).

%%% END of crone.erl %%%

format_task(Task) ->
    {When, {M,F,A}} = Task,
    Args = 
	case io_lib:format("~p", [A]) of
	    [[]] -> [];
	    [[_|Rest]] ->
		[_|Sgra] = lists:reverse(Rest),
		lists:reverse(Sgra)
	end,
    {case When of 
	 {daily, Period} ->
	     "daily " ++ format_period(Period);
	 {weekly, Dow, Period} -> 
	     Day = atom_to_list(Dow),
	     "Every " ++ Day ++ " at " ++ format_period(Period);
	 {monthly, Dom, Period} -> 
	     Day = integer_to_list(Dom),
	     "The " ++ Day ++ " every month at " ++ format_period(Period)
     end,
     atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "("
     ++ Args ++ ")"}.


format_period(Period) ->
    case Period of 
	L when is_list(L) -> lists:flatten([format_period(E) || E <- L]);
	{every, D, {between,Start,Stop}} ->
	    format_duration(D) ++ " between "
		++ format_time(resolve_time(Start))
		++ " and " 
		++ format_time(resolve_time(Stop));
	Time -> format_time(resolve_time(Time))
    end.

format_duration({1, Unit}) ->
    "every " ++ format_unit(Unit);
format_duration({2, Unit}) ->
    "every second " ++ format_unit(Unit);
format_duration({I, Unit}) ->
    "each " ++ integer_to_list(I) ++ " " ++ format_unit(Unit).

format_unit(hr) -> "hour";
format_unit(min) -> "minute";
format_unit(sec) -> "second".
    
