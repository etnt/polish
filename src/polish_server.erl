%%% @author Torbjorn Tornkvist <tobbe@kreditor.se>
%%% @copyright (C) 2010, Torbjorn Tornkvist

-module(polish_server).

-behaviour(gen_server).

%% API
-export([delete_old_locked_keys/0
	 , get_new_old_keys/0
	 , is_always_translated/2
	 , is_key_locked/2
	 , load_always_translated_keys/2
	 , load_po_files/1
	 , lock_keys/2
	 , mark_as_always_translated/2
	 , read_po_file/1
	 , set_new_old_keys/1
	 , unlock_user_keys/0
	 , update_po_file/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_link/0]).

-include("polish.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

load_po_files(CustomLCs) ->
    gen_server:call(?MODULE, {load_po_files, CustomLCs}).

read_po_file(LC) ->
    gen_server:call(?MODULE, {read_po_file, LC}).

update_po_file(LC, Changes) ->
    gen_server:call(?MODULE, {update_po_file, LC, Changes}).

lock_keys(KVs, LC) when is_atom(LC) ->
    gen_server:call(?MODULE, {lock_keys, KVs, LC, ?l2a(wf:user())}).

unlock_user_keys() ->
    case wf:user() of
	undefined -> ok;
	U -> gen_server:call(?MODULE, {unlock_user_keys, ?l2a(U)})
    end.

is_key_locked(Key, LC) when is_list(LC) ->
    is_key_locked(Key, ?l2a(LC));
is_key_locked(Key, LC) when is_atom(LC) ->
    gen_server:call(?MODULE, {is_key_locked, Key, LC, ?l2a(wf:user())}).

delete_old_locked_keys() ->
    gen_server:call(?MODULE, delete_old_locked_keys).

load_always_translated_keys(LC, File) ->
    gen_server:call(?MODULE, {load_always_translated_keys, LC, File}).

mark_as_always_translated(LC, Key) ->
    gen_server:call(?MODULE, {mark_as_always_translated, LC, Key}).

is_always_translated(LC, Key) ->
    gen_server:call(?MODULE, {is_always_translated, LC, Key}).

set_new_old_keys(NewOldKeys) ->
    gen_server:call(?MODULE, {set_new_old_keys, NewOldKeys}).

get_new_old_keys() ->
    gen_server:call(?MODULE, get_new_old_keys).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ets:new(?MODULE, [ordered_set,protected,{keypos,1},named_table]),
    ets:new(locked_keys, [ordered_set,protected,{keypos,1},named_table]),
    ets:new(always_translated, [ordered_set,protected,{keypos,1},named_table]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({load_po_files, CustomLCs}, _From, State) ->
    {NewState, Reply} = do_load_po_files(State, CustomLCs),
    {reply, Reply, NewState};

handle_call({read_po_file, LC}, _From, State) ->
    {NewState, Reply} = do_read_po_file(State, LC),
    {reply, Reply, NewState};

handle_call({update_po_file, LC, Changes}, _From, State) ->
    {NewState, Reply} = do_update_po_file(State, LC, Changes),
    {reply, Reply, NewState};

handle_call({lock_keys, KVs, LC, User}, _From, State) ->
    {NewState, Reply} = do_lock_keys(State, KVs, LC, User),
    {reply, Reply, NewState};

handle_call({unlock_user_keys, User}, _From, State) ->
    {NewState, Reply} = do_unlock_user_keys(State, User),
    {reply, Reply, NewState};

handle_call({is_key_locked, Key, LC, User}, _From, State) ->
    {NewState, Reply} = do_is_key_locked(State, Key, LC, User),
    {reply, Reply, NewState};

handle_call(delete_old_locked_keys, _From, State) ->
    {NewState, Reply} = do_delete_old_locked_keys(State),
    {reply, Reply, NewState};

handle_call({load_always_translated_keys, LC, File}, _From, State) ->
    {NewState, Reply} = do_load_always_translated_keys(State, LC, File),
    {reply, Reply, NewState};

handle_call({mark_as_always_translated, LC, Key}, _From, State) ->
    {NewState, Reply} = do_mark_as_always_translated(State, LC, Key),
    {reply, Reply, NewState};

handle_call({is_always_translated, LC, Key}, _From, State) ->
    {NewState, Reply} = do_is_always_translated(State, LC, Key),
    {reply, Reply, NewState};

handle_call({set_new_old_keys, NewOldKeys}, _From, State) ->
    {NewState, Reply} = do_set_new_old_keys(State, NewOldKeys),
    {reply, Reply, NewState};

handle_call(get_new_old_keys, _From, State) ->
    {NewState, Reply} = do_get_new_old_keys(State),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_load_po_files(State, CustomLCs) ->
    do_load_po_files(CustomLCs),
    {State, ok}.

do_read_po_file(State, LC) ->
    KVs = ets:select(?MODULE, [{{{LC,'_'}, {'$1','$2'}}, [], [{{'$1','$2'}}]}]),
    {State, KVs}.

do_update_po_file(State, LC, Changes) ->
    [ets:insert(?MODULE, {{LC,polish_utils:hash(K)}, {K,V}}) || {K,V}<-Changes],
    {State, ok}.

do_lock_keys(State, KVs, LC, User) ->
    Res = lists:foldl(
	    fun({Key, _Val} = KV, Acc) ->
		    case do_is_key_locked(State, Key, LC, User) of
			{State, true}  -> Acc;
			{State, false} ->
			    {Mega, Sec, _} = erlang:now(),
			    Time = Mega * 100000 + Sec,
			    ets:insert(locked_keys, {{Key, LC}, {User, Time}}),
			    [KV | Acc]
		    end
	    end, [], KVs),
    {State, Res}.

do_unlock_user_keys(State, User) ->
    [ets:delete(locked_keys, K) || {K, {U, _T}} <- ets:tab2list(locked_keys),
				   U =:= User],
    {State, result}.

do_is_key_locked(State, Key, LC, User) ->
    Res = case ets:lookup(locked_keys, {Key, LC}) of
	      []               -> false;
	      [{_K, {U, _T}}]  -> User =/= U
	  end,
    {State, Res}.

do_delete_old_locked_keys(State) ->
    {Mega, Sec, _} = erlang:now(),
    Time = Mega * 100000 + Sec - 60*30,
    R = ets:select(locked_keys, [{{{'$1', '$2'}, {'_','$3'}, '_'},
				  [{'<', '$3', Time}], [{{'$1', '$2'}}]}]),
    [ets:delete(locked_keys, K) || K <- R],
    {State, ok}.

do_load_always_translated_keys(State, LC, File) ->
    case file:consult(File) of
        {ok, List} ->
            lists:foreach(
              fun({always_translated, V}) ->
		      UnescV = unescape_key(V),
                      ets:insert(always_translated, {{LC, UnescV}, true});
                 ({_, _}) -> ok
              end, List),
            {State, ok};
        _ ->
            {State, ok}
    end.

do_mark_as_always_translated(State, LC, Key) ->
    ets:insert(always_translated, {{LC, Key}, true}),
    case file:open(polish:meta_filename(?a2l(LC)), [append]) of
        {ok,Fd}  ->
	    F = fun($", Acc)  -> [$\\,$"|Acc];
		   (C, Acc)   -> [C|Acc]
		end,
	    EscKey = lists:foldr(F, [], Key),
            Str = "{always_translated, \"" ++ EscKey ++ "\"}.\n",
            file:write(Fd, Str),
            {State, ok};
        _ ->
            {State, ok}
    end.

do_is_always_translated(State, LC, Key) ->
    Res = case ets:lookup(always_translated, {LC, Key}) of
	      [] -> false;
	      _  -> true
    end,
    {State, Res}.

do_set_new_old_keys(State, NewOldKeys) ->
    put(new_old_keys, NewOldKeys),
    {State, ok}.

do_get_new_old_keys(State) ->
    {State, get(new_old_keys)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  I N T E R N A L   F U N C T I O N S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_load_po_files([LC|CustomLCs]) ->
    KVs = polish_wash:read_po_file(LC),
    [ets:insert(?MODULE, {{LC, polish_utils:hash(K)}, {K, V}}) || {K,V} <- KVs],
    assure_po_file_loaded_correctly(LC, KVs),
    do_load_po_files(CustomLCs);
do_load_po_files([]) ->
    ok.

assure_po_file_loaded_correctly(LC, KVs) ->
    StoredKVs = ets:select(?MODULE, [{{{LC, '_'}, {'$1','$2'}},
				   [], [{{'$1', '$2'}}]}]),
    KVs = lists:sort(StoredKVs).

unescape_key(Str) ->
    unescape_key(Str, []).
unescape_key([$\\,$"|Str], Acc) -> unescape_key(Str, [$"|Acc]);
unescape_key([Ch|Str], Acc)     -> unescape_key(Str, [Ch|Acc]);
unescape_key([], Acc)           -> lists:reverse(Acc).
