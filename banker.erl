%% A Banker in the Banker's Algorithm.
%% Author: Andrew Garrett
-module(banker).
-export([start/1, status/0, attach/1, request/1, release/1]).
-import_record_info([{client, client}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Banker data structure
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The Banker's data.
%%  capital: the initial capital of the bank; the total amount of resources
%%           which will be available
%%  cash_on_hand: the amount of resources not yet lent.
%%  clients: a list of Clients (their pids)
-record(banker,
        { capital :: non_neg_integer()
        , cash_on_hand :: non_neg_integer()
        , clients :: list(pid())
        }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Client functions (send messages to the server)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% start/1
%% Spawns the Banker process and registers it with the atom banker.
%% Arguments:
%%  Capital - the specified capital amount with which to begin
start(Capital) ->
    Banker = #banker{capital = Capital, cash_on_hand = Capital},
    register(banker, spawn(banker, main, [Banker])).

%% status/0
%% Reports the status of the system.
%% Returns:
%%  {Capital, CashOnHand, NClients}
status() ->
    case whereis(banker) of
        unregistered ->
            throw(banker_not_registered);
        _ ->
            banker ! {self(), status},
            receive
                Any -> Any
            end
    end.

%% attach/1
%% A Client attaches to the Banker.
%% Arguments:
%%  Limit: the maximum number of resources the Client can request.
attach(Limit) ->
    case whereis(banker) of
        unregistered ->
            throw(banker_not_registered);
        _ ->
            banker ! {self(), attach, Limit},
            receive
                Any -> Any
            end
    end.

%% request/1
%% An attached Client requests more resources from the Banker.
%% Arguments:
%%  NUnits: the number of resources requested.
request(NUnits) ->
    case whereis(banker) of
        unregistered ->
            throw(banker_not_registered);
        _ ->
            banker ! {self(), request, NUnits}
            receive
                Any -> Any
            end
    end.

%% release/1
%% An attached Client releases resources to the Banker.
%% Arguments:
%%  NUnits: the number of resources released.
release(NUnits) ->
    case whereis(banker) of
        unregistered ->
            throw(banker_not_registered);
        _ ->
            banker ! {self(), release, NUnits},
            receive
                Any -> Any
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Server functions (run the bank)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(Banker) ->
    process_flag(trap_exit, true),
    Capital = Banker#banker.capital,
    CashOnHand = Banker#banker.cash_on_hand,
    Clients = Banker#banker.clients,
    receive
        {Pid, status} ->
            NewBanker = Banker,
            Pid ! { Capital, CashOnHand, length(Clients)};
        {Pid, attach, Limit} ->
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand
                                , clients = [Pid | Clients]
                                },
            link(Pid);
        {Pid, request, NUnits} ->
            % How to get #clients instead of pids?
            lists:sort(compare_clients, Clients),
            case is_safe_state(Clients, CashOnHand) of
                true ->
                    Pid ! ok,
                    NewBanker = #banker { capital = Capital
                                        , cash_on_hand = CashOnHand - NUnits
                                        , clients = Clients
                                        };
                false ->
                    
            end;
        {Pid, release, NUnits} ->
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand + NUnits
                                , clients = Clients
                                };
            % determine whether any outstanding requests can be granted
        {'EXIT', Pid, Reason} ->
            % reclaim the loan
            NewBanker = #banker { capital = Capital
                                , cash_on_hand =
                                , clients = delete(Pid, Clients)
                                };
        _ ->
            throw(unexpected_banker_message)
    end,
    main(NewBanker).
    
%% compare_clients/2
%% Compare two clients based on remaining claim.
%% Arguments:
%%  C1: a Client record
%%  C2: a different Client record
compare_clients(C1, C2) ->
    C1#client.claim < C2#client.claim.

%% is_safe_state/2
%% Check the list of Clients and determine if the state is safe.
%% Arguments:
%%  Clients: the list of Clients.
%%  NUnits: the number of resources requested by a Client.
%% Returns:
%%  true if state is safe, false is not.
is_safe_state([], _) ->
    true;
is_safe_state([CH | CT], CashOnHand) ->
    if
        CH.client#claim > CashOnHand ->
            false;
        CH.client#claim =< CashOnHand ->
            is_safe_state(CT, CashOnHand + CH.client#loan)
    end.