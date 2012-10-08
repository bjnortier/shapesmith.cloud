-module(warroom_collector).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, register_listener/1, deregister_listener/1]).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call({global, ?MODULE}, stop).

register_listener(ListenerPid) ->
    gen_server:call({global, ?MODULE}, {register_listener, ListenerPid}).

deregister_listener(ListenerPid) ->
    gen_server:call({global, ?MODULE}, {deregister_listener, ListenerPid}).

%%%===================================================================
%%% gen_server
%%%===================================================================

-record(state, {listeners}).

init([]) ->
    {ok, #state{listeners = []}}.

handle_call({new_gauge, Node, Name}, _From, State) ->
    folsom_metrics:new_gauge({Node, Name}),
    {reply, ok, State};
handle_call({new_history, Node, Name}, _From, State) ->
    folsom_metrics:new_history({Node, Name}),
    {reply, ok, State};
handle_call({register_listener, Listener}, _From, State = #state { listeners = Listeners }) ->
    {reply, ok, State#state{ listeners = [Listener|Listeners]}};
handle_call({deregister_listener, Listener}, _From, State = #state { listeners = Listeners }) ->
    {reply, ok, State#state{ listeners = lists:delete(Listener, Listeners)}};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(Request, _From, State) ->
    lager:warning("~p - unsupported call: ~p", [?MODULE, Request]),
    {reply, unknown_call, State}.

handle_cast({notify, Node, Name, Value}, State = #state { listeners = Listeners }) ->
    folsom_metrics:notify({{Node, Name}, Value}),
    lists:map(fun(Listener) ->
                Listener ! {notify, {Node, Name}, Value}
        end,
        Listeners),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:warning("~p - unsupported message: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



