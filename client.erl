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
            Client = #client{limit = Limit, claim = Limit},
            io:format(  "(client start) A new Client is being spawned with"
                        " limit = ~p.~n"
                        ,[Limit]),
            Loop = fun(C, X) ->
                io:format(  "(client start) Client ~p is attaching to the"
                            " Banker with a limit of ~p.~n"
                            , [self(), C#client.limit]),
                banker:attach(C#client.limit),
                % Don't let client start running until it's fully attached.
                receive
                    {attached} -> ok
                end,
                io:format(  "(client start) Client ~p attached to Banker.~n"
                            , [self()]),
                client_loop(C, X)
                end,
            % suggestion from http://stackoverflow.com/a/16113499/691859
            spawn(fun() -> process_flag(trap_exit, true), Loop(Client, N) end)
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
    NewClient = try
        io:format("(client_loop) Client ~p is on iteration ~p.~n", [self(), N]),
        random:seed(now()),
        case random:uniform(2) of
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
        end
    catch
        exit:Reason ->
            io:format(  "(client_loop) Client ~p caught exit for the following"
                        " reason: ~p, before it was able to complete its"
                        " request.~n", [self(), Reason]),
            exit({terminated, Client#client.loan})
    end,
    receive
        {'EXIT', FromPid, Reason2} ->
            io:format(  "(client_loop) Client ~p is being terminated for the"
                        " following reason: ~p. Has loan of: ~p.~n"
                        , [FromPid, Reason2, NewClient#client.loan]),
            exit({terminated, NewClient#client.loan})
    after 0 ->
        io:format(  "(client_loop) Client ~p iteration ~p complete.~n",
                    [self(), N]),
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
    io:format(  "(request) Client ~p is requesting ~p resources.~n"
                , [self(), NUnits]),
    banker:request(NUnits),
    io:format(  "(request) Client ~p is prepared to receive state requests.~n"
                , [self()]),
    % It is necessary for the Banker to get Client state from all clients.
    % Only the client which issued this request will get the polling_done msg.
    % Others will wait until their request gets processed.
    receive_state_requests(Client),
    receive
        ok ->
        % Request can be satisfied, update your state and proceed normally.
            io:format(  "(request) Client ~p request for ~p resources accepted."
                        "~n"
                        , [self(), NUnits]),
            io:format(  "(request) Client ~p now has ~p resources.~n"
                        , [self(),Client#client.loan + NUnits]),
            #client { limit = Client#client.limit
                                , loan = Client#client.loan + NUnits
                                , claim = Client#client.claim - NUnits
                                };
        {Pid, unsafe} ->
        % Request cannot be satisfied. Mark yourself as waiting.
            io:format(  "(request) Client ~p request for ~p resources denied.~n"
                        , [self(), NUnits]),
            Pid ! {self(), waiting},
            io:format("(request) Client ~p is waiting.~n", [self()]),
            receive
            % could this block?
                try_again ->
                    io:format(  "(request) Client ~p is trying to request ~p"
                                " resources again.~n"
                                , [self(), NUnits]),
                    request(Client, NUnits)
            end
    end.
    
%% release/2
%% Send a release request to the Banker
%% Arguments:
%%  Client - the #client record
%%  NUnits - the number of resources to release
%% Returns:
%%  the modified #client record (i.e., NewClient)
release(Client, NUnits) ->
    io:format(  "(release) Client ~p is releasing ~p resources.~n"
                , [self(), NUnits]),
    banker:release(NUnits),
    io:format(  "(release) Client ~p now has ~p resources.~n"
                , [self(), Client#client.loan - NUnits]),
    #client { limit = Client#client.limit
            , loan = Client#client.loan - NUnits
            , claim = Client#client.claim + NUnits
            }.

%% receive_state_requests
%% Continually process requests for Client state until the polling_done message
%% is received. The purpose of this function is to make sure that a Client
%% will return its state when the Banker needs it.
%% Arguments:
%%  Client - the #client record
receive_state_requests(Client) ->
    receive
        {Pid, getclaim} ->
            io:format(  "(receive_state_requests) Banker requesting claim from"
                        " Client ~p. Claim is ~p.~n"
                        , [self(), Client#client.claim]),
            Pid ! {self(), claim, Client#client.claim},
            receive_state_requests(Client);
        {Pid, getloan} ->
            io:format(  "(receive_state_requests) Banker requesting loan from"
                        " Client ~p. Claim is ~p.~n"
                        , [self(), Client#client.loan]),
            Pid ! {self(), loan, Client#client.loan},
            receive_state_requests(Client);
        {_Pid, polling_done} ->
            io:format(  "(receive_state_requests) Client ~p may move forward.~n"
                        , [self()]),
            done
    end.