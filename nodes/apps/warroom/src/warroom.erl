-module(warroom).
-author('Benjamin Nortier <bjnortier@shapesmith.net>').
-export([start/0]).
-export([new_gauge/1, new_history/1, notify/1]).

%%%===================================================================
%%% API
%%%===================================================================

ensure_started(App) ->
    case application:start(App) of
        ok -> 
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
    ensure_started(lager),
    ensure_started(folsom),
    ensure_started(cowboy),
    application:start(warroom).

new_gauge(Name) ->
    folsom_metrics:new_gauge(Name),
    collect(call, {new_gauge, node(), Name}).

new_history(Name) ->
    folsom_metrics:new_history(Name),
    collect(call, {new_history, node(), Name}).

notify({Name, Value}) ->
    folsom_metrics:notify({Name, Value}),
    collect(cast, {notify, node(), Name, Value}).

%%%===================================================================
%%% Private
%%%===================================================================

collect(Type, Msg) ->
    case global:whereis_name(warroom_collector) of 
        undefined ->
            lager:error("No warroom found in cluster", []),
            {error, no_warroom};
        _Pid ->
            gen_server:Type({global, warroom_collector}, Msg)
    end.
