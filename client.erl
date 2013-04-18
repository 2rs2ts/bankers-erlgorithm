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
            case Limit > Capital of
                true -> throw(client_limit_too_high)
            end,
            Client = #client{limit = Limit, claim = Limit},
            spawn(fun() -> client_loop(Client, N) end)
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
client_loop(_, 0) ->
    io:format("Client ~p is exiting.~n", [self()]),
    exit(finished);
client_loop(Client, N) ->
    receive
        {Pid, getclient} ->
            Pid ! Client
    after 0 ->
        case whereis(banker) of
            unregistered ->
                throw(banker_not_registered);
            _ ->
                io:format(  "Client ~p is attaching to the Bank with a limit of
                            ~p.~n",
                            [self(), Client#client.limit]),
                banker:attach(Client#client.limit),
                {Capital, _, _} = banker:status()
        end,
        NUnits = random:uniform(Capital),
        case random:uniform(2) of
            1 ->    % Request
                io:format(  "Client ~p is requesting ~p resources.~n",
                            [self(), NUnits]),
                banker:request(NUnits),
                NewClient = request(Client, NUnits);
            2 ->    % Release
                io:format(  "Client ~p is releasing ~p resources.~n",
                            [self(), NUnits]),
                banker:release(NUnits),
                NewClient = #client { limit = Client#client.limit
                                    , loan = Client#client.loan - NUnits
                                    , claim = Client#client.claim + NUnits
                                    };
        end
    end,
    client_loop(NewClient, N-1).

%% request/2
%% Send a loan request to the Bank
%% Arguments:
%%  Client: the #client record
%%  NUnits: the number of resources requested
%% Returns:
%%  NewClient: the modified #client record
request(Client, NUnits) ->
    receive
        ok ->
            io:format(  "Client ~p request for ~p resources accepted.~n",
                        [self(), NUnits]),
            NewClient = #client { limit = Client#client.limit
                                , loan = Client#client.loan + NUnits
                                , claim = Client#client.claim - NUnits
                                };
        {Pid, unsafe} ->
            Pid ! {self(), waiting},
            io:format(  "Client ~p request for ~p resources denied,
                        Client is waiting.~n",
                        [self(), NUnits]),
            receive
                try_again ->
                    io:format(  "Client ~p is trying to request ~p resources
                                again.~n",
                                [self(), NUnits]),
                    request(Client, NUnits) % not tail recursive!
            end;
        _ ->
            throw(unexpected_client_message)
    end,
    NewClient.
