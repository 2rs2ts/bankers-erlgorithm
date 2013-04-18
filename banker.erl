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
        , client_procs :: list(pid())
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
                {Capital, CashOnHand, NClients} ->
                    {Capital, CashOnHand, NClients}
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
            banker ! {self(), attach, Limit}
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
            banker ! {self(), release, NUnits}
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
    ClientProcs = Banker#banker.client_procs,
    receive
        {Pid, status} ->
            NewBanker = Banker,
            Pid ! { Capital, CashOnHand, length(ClientProcs)};
        {Pid, attach, Limit} ->
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand
                                , client_procs = [Pid | ClientProcs]
                                },
            link(Pid);
        {Pid, request, NUnits} ->
            Clients = get_clients(ClientProcs),
            lists:sort(compare_clients, Clients),
            case is_safe_state(Clients, CashOnHand) of
                true ->
                    Pid ! ok,
                    NewBanker = #banker { capital = Capital
                                        , cash_on_hand = CashOnHand - NUnits
                                        , clients = ClientProcs
                                        };
                false ->
                    Pid ! unsafe
            end;
        {Pid, release, NUnits} ->
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand + NUnits
                                , clients = ClientProcs
                                };
            % determine whether any outstanding requests can be granted
            notify_waiting_clients();
        {'EXIT', Pid, Reason} ->
            % reclaim the loan
            NewBanker = #banker { capital = Capital
                                , cash_on_hand =
                                , clients = delete(Pid, ClientProcs)
                                }
    after 0 ->
        %_ ->
        %    throw(unexpected_banker_message)
    end,
    main(NewBanker).

%% get_clients/1
%% Get the #client records from a list of Client processes.
%% Arguments:
%%  ClientProcs: the list of Client processes.
%% Returns:
%%  Clients: the list of #client records.
get_clients(ClientProcs) -> h_get_clients(ClientProcs, []).
h_get_clients([], Clients) -> Clients;
h_get_clients([PH | PT], Clients) ->
    PH ! {self(), getclient},
    receive
        Client ->
            NewClients = [Client | Clients]
    end,
    h_get_clients(PT, NewClients).

    
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

%% notify_waiting_clients/0
%% Go through the mailbox and find all messages from Client procs which are
%% waiting to have their requests processed, and tell them to try_again.
%% Returns:
%%  ok when done.
notify_waiting_clients() ->
    receive
        {Pid, waiting} ->
            Pid ! try_again,
            notify_waiting_clients()
    after 0 ->
        ok
    end.