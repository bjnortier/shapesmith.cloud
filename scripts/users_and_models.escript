#!/usr/bin/env escript

main(_) ->
    application:start(inets),
    {ok, {_, _, Response}} = httpc:request(get, {"http://localhost:8098/buckets?buckets=true", []}, [], []),
    code:add_path("/home/ubuntu/shapesmith.cloud/nodes/deps/jiffy/ebin"),
    code:add_path("/Users/bjnortier/development/shapesmith.cloud/nodes/deps/jiffy/ebin"),
    {Props} = jiffy:decode(list_to_binary(Response)),
    {_, Buckets} = lists:keyfind(<<"buckets">>, 1, Props),
    {UserModels, NumModels} = lists:foldl(
        fun(BucketBin, {UserModels, NumModels}) ->
            Bucket = binary_to_list(BucketBin),
            case string:tokens(Bucket, "/") of
                [Username] -> 
                    case dict:find(Username, UserModels) of
                        {ok, _} ->
                            %% Do nothing. User is already in the dictionaty
                            {UserModels, NumModels};
                        error ->
                            {dict:store(Username, [], UserModels), NumModels}
                    end;
                [Username, ModelName] ->
                    case dict:find(Username, UserModels) of
                        {ok, _} ->
                            {dict:append(Username, ModelName, UserModels), NumModels + 1};
                        error ->
                            {dict:store(Username, [ModelName], UserModels), NumModels + 1}
                    end;
                _Err ->
                    throw({unsupported_bucket, Bucket})
            end
        end,
        {dict:new(), 0},
        Buckets),
    Usernames = dict:fetch_keys(UserModels),
    TempUsers = lists:filter(
        fun(Username) ->
            string:str(Username, "___temp") =/= 0
        end,
        Usernames),
    NumUsers = length(Usernames) - length(TempUsers),
    io:format("number of users: ~p~n", [NumUsers]),
    io:format("number of temp users: ~p~n", [length(TempUsers)]),
    io:format("number of models: ~p~n", [NumModels]),
    {{Y,M,D},{HH,MM,SS}} = calendar:universal_time(),
    URL = lists:flatten(io_lib:format("http://localhost:8098/buckets/_dailystats/keys/~p_~p_~p_~p_~p_~p", [Y,M,D,HH,MM,SS])),
    Contents = jiffy:encode({[{<<"users">>, NumUsers}, {<<"temp_users">>, length(TempUsers)}, {<<"models">>, NumModels}]}),
    io:format("storing ~p ~p", [URL, Contents]),
    {ok, {{_, 204, _}, _, _}} = httpc:request(put, {URL, [], "application/json", Contents}, [], []).
            
