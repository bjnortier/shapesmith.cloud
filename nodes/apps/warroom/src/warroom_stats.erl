-module(warroom_stats).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, add_client/1, remove_client/1, get_metrics/0, get_daily_stats/0]).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

add_client(Pid) ->
    gen_server:call(?MODULE, {add, Pid}).

remove_client(Pid) ->
    gen_server:call(?MODULE, {remove, Pid}).

get_metrics() ->
    gen_server:call(?MODULE, all_metrics).

get_daily_stats() ->
    gen_server:call(?MODULE, daily_stats).


%%%===================================================================
%%% gen_server
%%%===================================================================

-record(state, {clients=[], daily_stats}).

init([]) ->
    ok = warroom_collector:register_listener(self()),
    DailyStats = fetch_daily_stats(),
    {Date, _} = calendar:universal_time(),
    {ok, #state{daily_stats={Date, DailyStats}}}.

handle_call({add, Pid}, _From, State = #state{ clients = Clients }) ->
    {reply, ok, State#state{ clients = [Pid|Clients]}};
handle_call({remove, Pid}, _From, State = #state{ clients = Clients }) ->
    {reply, ok, State#state{ clients = lists:delete(Pid, Clients)}};
handle_call(all_metrics, _From, State) ->
    LocalAndGlobal = folsom_metrics:get_metrics(),
    Global = lists:filter(fun({_Node, _Metric}) ->
                    true;
                (_) ->
                    false
            end,
            LocalAndGlobal),
    Reply = lists:map(fun({Node, MetricName}) -> 
                    {Node, MetricName, initial_value({Node, MetricName})}
            end,
            Global),
    {reply, Reply, State};
handle_call(daily_stats, _From, State=#state{daily_stats = {CachedDate, CachedDailyStats}}) ->
    {Today, _} = calendar:universal_time(),
    {Reply, NewState} = case CachedDate of
        Today -> 
            {CachedDailyStats, State};
        _ ->
            NewDailyStats = fetch_daily_stats(),
            {NewDailyStats, State#state{ daily_stats = {Today, NewDailyStats}}}
        end,
    {reply, Reply, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({notify,{Node, MetricName}, Value}, State = #state{ clients = Clients }) ->
    lists:map(
        fun(Client) ->
                Client ! {stats, Node, MetricName, Value}
        end, Clients),
    {noreply, State};
handle_info(Info, State) ->
    lager:warning("unknown info to ~p:~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = warroom_collector:deregister_listener(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% private
%%%===================================================================

initial_value({Node, MetricName}) ->
    case folsom_metrics:get_metric_info({Node, MetricName}) of
        [{{Node, MetricName},[{type,history}]}] -> 
            folsom_metrics:get_history_values({Node, MetricName}, 100);
        _ ->
            folsom_metrics:get_metric_value({Node, MetricName})
    end.

fetch_daily_stats() ->
    {ok, Client} = riakc_pb_socket:start_link("localhost", 8087),
    {Today, _} = calendar:universal_time(),
    TodayGregorianDays = calendar:date_to_gregorian_days(Today),
    Stats = lists:foldr(fun(X, Acc) ->
                if 
                    length(Acc) =:= 7 ->
                        Acc;
                    true ->
                        {Y,M,D} = calendar:gregorian_days_to_date(TodayGregorianDays - X),
                        Key = iolist_to_binary(io_lib:format("~p_~p_~p", [Y,M,D])),
                        case riakc_pb_socket:get(Client, <<"_dailystats">>, Key) of
                            {error, notfound} ->
                                Acc;
                            {ok, Obj} ->
                                Result = riakc_obj:get_value(Obj),
                                [jiffy:decode(Result)|Acc]
                        end
                end
            end,
            [],
            lists:seq(0,8)),
    Stats.


