-module(warroom_websocket).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

%%%===================================================================
%%% gen_websocket (not an actual behaviour, but very similar)
%%%===================================================================

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    warroom_stats:add_client(self()),
    self() ! send_metrics,
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.



websocket_info(send_metrics, Req, State) ->
    GlobalMetrics = warroom_stats:get_metrics(),
    DailyStats = warroom_stats:get_daily_stats(),
    JSON = {[{<<"all">>, 
                lists:map(fun({Node, Metric, Value}) ->
                        {[
                            {<<"node">>, atom_to_binary(Node, utf8)}, 
                            {<<"metric">>, atom_to_binary(Metric, utf8)},
                            {<<"value">>, metric_value_to_json(Value)}
                        ]}
                end,
                GlobalMetrics)
            },
            {<<"daily">>, DailyStats}]},
    {reply, {text, jiffy:encode(JSON)}, Req, State};
websocket_info({stats, Node, MetricName, MetricValue}, Req, State) ->
    JSON = {[{<<"value">>, 
                {[
                    {<<"node">>, list_to_binary(atom_to_list(Node))},
                    {<<"metric">>, atom_to_binary(MetricName, utf8)},
                    {<<"value">>, MetricValue}
                ]}
            }]},
    {reply, {text, jiffy:encode(JSON)}, Req, State};
websocket_info(Info, Req, State) ->
    lager:warning("~p - unsupported messageL ~p", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    warroom_stats:remove_client(self()), 
    ok.


%%%===================================================================
%%% private
%%%===================================================================

metric_value_to_json(ListValue) when is_list(ListValue) ->
    %% Only history supported
    lists:foldl(fun({Timestamp, Events}, FlatEvents) ->
                EventsForTimestamp = lists:foldr(fun({event, Value}, Acc) ->
                                [{[{<<"timestamp">>, Timestamp}, {<<"value">>, Value}]}|Acc]
                        end,
                        [],
                        Events),
                FlatEvents ++ EventsForTimestamp
        end,
        [],
        ListValue);
metric_value_to_json(Value) ->
    Value.


%%%===================================================================
%%% tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

metric_value_to_json_test_() ->
    ListValue1 = [{125,[{event,2}]}, {122,[{event,1}]}],
    ListValue2 = [{125,[{event, 1}, {event,2}]}, {120,[{event,3}]}],
    [
        ?_assertEqual([{[{<<"timestamp">>,125},{<<"value">>,2}]},
                {[{<<"timestamp">>,122},{<<"value">>,1}]}], 
            metric_value_to_json(ListValue1)),
        ?_assertEqual([{[{<<"timestamp">>,125},{<<"value">>,1}]},
                {[{<<"timestamp">>,125},{<<"value">>,2}]},
                {[{<<"timestamp">>,120},{<<"value">>,3}]}], 
            metric_value_to_json(ListValue2))
        ].

-endif.
