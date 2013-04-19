%% A client in the Banker's Algorithm.
%% Author: Andrew Garrett
-module(client).
-export([start/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Client data structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The Client's data.
%%  limit: the maximum number of resources the Client can request.
%%  loan: the number of resources currently allocated to the Client.
%%  claim: the number of resources which the Client may request.
-record(client,
        { limit :: non_neg_integer()
        , loan = 0 :: non_neg_integer()
        , claim :: non_neg_integer()
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Client functions (instantiate a new Client)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% start/2
%% Spawn a new Client process.
%% Arguments:
%%  Limit: the maximum number of resources the Client can request.
%%  N: the number of interactions in which the Client will engage.
start(Limit, N) ->
    case whereis(banker) of
        unregistered ->
            throw(banker_not_registered);
        _ ->
            {Capital, _, _} = banker:status(),
            if
                Limit > Capital ->
                    throw(client_limit_too_high);
                Limit =< Capital ->
                    Client = #client{limit = Limit, claim = Limit},
                    io:format(  "(client start) A new Client is being spawned with limit = ~p.
                                ~n"
                                ,[Limit]),
                    ClientLoop = fun(C, X) ->
                        io:format(  "(client start) Client ~p is attaching to the Banker with a limit of
                            ~p.~n"
                            , [self(), C#client.limit]),
                        banker:attach(C#client.limit),
                        io:format("(client start) Client ~p attached to Banker.~n", [self()]),
                        client_loop(C, X)
                        end,
                    spawn(fun() -> ClientLoop(Client, N) end)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Server functions (run the client)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% client_loop/2
%% Run the client.
%% Arguments:
%%  Client: the #client record
%%  N: the number of operations which will be performed before exiting
client_loop(Client, 0) ->
    io:format("(client_loop) Client ~p is exiting.~n", [self()]),
    exit({finished, Client#client.loan});
client_loop(Client, N) ->
    io:format("(client_loop) Client ~p is on iteration ~p.~n", [self(), N]),
    receive
        %{Pid, getclient} ->
        %    Pid ! {client, Client};
        {Pid, getclaim} ->
            io:format("(client_loop) Banker requesting claim from Client ~p.~n", [self()]),
            Pid ! {claim, Client#client.claim};
            %client_loop(Client, N);
        {Pid, getloan} ->
            io:format("(client_loop) Banker requesting loan from Client ~p.~n", [self()]),
            Pid ! {loan, Client#client.loan}
            %client_loop(Client, N)
    after 0 ->
        %% need to do this only once...
        %Capital = case whereis(banker) of
        %    unregistered ->
        %        throw(banker_not_registered);
        %    _ ->
        %        io:format(  "Client ~p is attaching to the Bank with a limit of
        %                    ~p.~n"
        %                    , [self(), Client#client.limit]),
        %        banker:attach(Client#client.limit),
        %        {TheCapital, _, _} = banker:status(),
        %        TheCapital
        %end,
        %NUnits = random:uniform(Capital),
        random:seed(now()),
        NewClient = case random:uniform(2) of
            % Normal cases
            1 when Client#client.claim > 0 ->
                NUnits = random:uniform(Client#client.claim),
                request(Client, NUnits);
            2 when Client#client.loan > 0 ->
                NUnits = random:uniform(Client#client.loan),
                release(Client, NUnits);
            % If guards fail
            1 when Client#client.claim == 0 ->
                NUnits = random:uniform(Client#client.loan),
                release(Client, NUnits);
            2 when Client#client.loan == 0 ->
                NUnits = random:uniform(Client#client.claim),
                request(Client, NUnits)
        end,
        client_loop(NewClient, N-1)
    end.

%% request/2
%% Send a loan request to the Banker
%% Arguments:
%%  Client: the #client record
%%  NUnits: the number of resources requested
%% Returns:
%%  the modified #client record (i.e., NewClient)
request(Client, NUnits) ->
    io:format("(request) Client ~p is requesting ~p resources.~n", [self(), NUnits]),
    banker:request(NUnits),
    receive
        {Pid, getclaim} ->
            io:format("(request) Banker requesting claim from Client ~p.~n", [self()]),
            Pid ! {claim, Client#client.claim}
            %client_loop(Client, N);
    end,
    receive
        {Pid1, getloan} ->
            io:format("(request) Banker requesting loan from Client ~p.~n", [self()]),
            Pid1 ! {loan, Client#client.loan}
            %client_loop(Client, N)
    end,
        receive
            ok ->
                io:format(  "(request) Client ~p request for ~p resources accepted.~n"
                            , [self(), NUnits]),
                io:format("(request) Client ~p now has ~p resources.~n", [self(),Client#client.loan + NUnits]),
                #client { limit = Client#client.limit
                                    , loan = Client#client.loan + NUnits
                                    , claim = Client#client.claim - NUnits
                                    };
            {Pid2, unsafe} ->
                io:format(  "(request) Client ~p request for ~p resources denied.~n"
                            , [self(), NUnits]),
                Pid2 ! {self(), waiting},
                io:format(  "(request) Client ~p is waiting.~n"
                            , [self()]),
                receive
                    try_again ->
                        io:format(  "(request) Client ~p is trying to request ~p resources
                                    again.~n"
                                    , [self(), NUnits]),
                        request(Client, NUnits) % not tail recursive!
                end
        %end
    %after 0 ->
        %_ ->
        %    throw(unexpected_client_message)
    end.
    
%% release/2
%% Send a release request to the Banker
%% Arguments:
%%  Client - the #client record
%%  NUnits - the number of resources to release
%% Returns:
%%  the modified #client record (i.e., NewClient)
release(Client, NUnits) ->
    io:format("(release) Client ~p is releasing ~p resources.~n", [self(), NUnits]),
    banker:release(NUnits),
    io:format("(release) Client ~p now has ~p resources.~n", [self(),Client#client.loan - NUnits]),
    #client { limit = Client#client.limit
            , loan = Client#client.loan - NUnits
            , claim = Client#client.claim + NUnits
            }.
