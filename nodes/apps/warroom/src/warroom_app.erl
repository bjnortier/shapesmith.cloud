-module(warroom_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = warroom_sup:start_link(),
    
    Dispatch = [
            {'_', [
                    {[], cowboy_http_static, [
                            {directory, {priv_dir, warroom, []}},
                            {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
                            {file, <<"index.html">>}]},
                    {[<<"ws">>], warroom_websocket, []},
                    {['...'], cowboy_http_static, [
                            {directory, {priv_dir, warroom, []}},
                            {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                            ]}
                    
                    ]}
            ],
    cowboy:start_listener(my_http_listener, 100,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]),
    {ok, Pid}.

stop(_State) ->
    ok.
