%% A Banker in the Banker's Algorithm.
%% Author: Andrew Garrett
-module(banker).
-export([start/1, status/0, attach/1, request/1, release/1]).

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
        , client_procs = [] :: list(pid())
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
    register(banker, spawn(fun() -> main(Banker) end)),
    io:format("Banker started. Pid = ~p.~n", [whereis(banker)]).

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

%% main/1
%% Run the Bank.
%% Arguments:
%%  Banker: the #banker record
main(Banker) ->
    process_flag(trap_exit, true),
    Capital = Banker#banker.capital,
    CashOnHand = Banker#banker.cash_on_hand,
    ClientProcs = Banker#banker.client_procs,
    receive
        {'EXIT', Pid, {finished, Loan}} ->
        % A normal exit by a Client.
            io:format(  "(main) Banker reclaims ~p resources from exiting"
                        " Client ~p.~n"
                        , [Loan, Pid]),
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand + Loan
                                , client_procs = lists:delete(Pid, ClientProcs)
                                },
            io:format(  "(main) Banker status after exit: CashOnHand = ~p," 
                        " ClientProcs = ~p.~n"
                        , [NewBanker#banker.cash_on_hand
                        , NewBanker#banker.client_procs]),
            main(NewBanker);
        {'EXIT', Pid, {terminated, Loan}} ->
        % An abnormal exit (e.g. an external exit call) by a Client.
            io:format(  "(main) Banker reclaims ~p resources from terminated"
                        " Client ~p.~n"
                        , [Loan, Pid]),
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand + Loan
                                , client_procs = lists:delete(Pid, ClientProcs)
                                },
            io:format(  "(main) Banker status after exit: CashOnHand = ~p,"
                        " ClientProcs = ~p.~n"
                        , [NewBanker#banker.cash_on_hand
                        , NewBanker#banker.client_procs]),
            main(NewBanker);
        {Pid, status} ->
            io:format(  "(main) Banker status was requested. CashOnHand = ~p,"
                        " ClientProcs = ~p.~n"
                        , [CashOnHand, ClientProcs]),
            Pid ! { Capital, CashOnHand, length(ClientProcs)},
            main(Banker);
        {Pid, attach, Limit} when Limit =< Capital ->
            io:format("(main) Client ~p attaching to Banker.~n", [Pid]),
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand
                                , client_procs = [Pid | ClientProcs]
                                },
            link(Pid),
            io:format("(main) Client ~p linked to Banker.~n", [Pid]),
            % Acknowledge the finished attachment.
            Pid ! {attached},
            main(NewBanker);
        {_Pid, attach, _Limit} ->
            throw(limit_exceeds_capital);
        {Pid, request, NUnits} ->
            io:format(  "(main) Client ~p requesting ~p resources from"
                        " Banker.~n"
                        , [Pid, NUnits]),
            Compare_Clients = fun(C1, C2) -> compare_clients(C1, C2) end,
            io:format("(main) Banker is sorting clients: ~p.~n", [ClientProcs]),
            SortedClients = lists:sort(Compare_Clients, ClientProcs),
            io:format("(main) Banker sorted clients: ~p.~n", [SortedClients]),
            io:format("(main) Banker is checking for safe state.~n", []),
            NewBanker = case is_safe_state(SortedClients, CashOnHand) of
                true ->
                    io:format("(main) State is safe.~n", []),
                    % Let the Client which made this request proceed.
                    % Others must wait until their request is processed.
                    Pid ! {self(), polling_done},
                    Pid ! ok,
                    #banker { capital = Capital
                            , cash_on_hand = CashOnHand - NUnits
                            , client_procs = SortedClients
                            };
                false ->
                    io:format("(main) State is not safe.~n", []),
                    Pid ! {self(), polling_done},
                    Pid ! {self(), unsafe},
                    Banker
            end,
            io:format(  "(main) Banker status is now: CashOnHand = ~p,"
                        " ClientProcs = ~p.~n"
                        , [NewBanker#banker.cash_on_hand
                        , NewBanker#banker.client_procs]),
            main(NewBanker);
        {Pid, release, NUnits} ->
            io:format(  "(main) Client ~p releasing ~p resources from Banker.~n"
                        , [Pid, NUnits]),
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand + NUnits
                                , client_procs = ClientProcs
                                },
            io:format(  "(main) Banker is notifying waiting Clients to try"
                        " again.~n"
                        , []),
            % Released resources may have created a safe state.
            % Notify waiting Clients that they should try again.
            notify_waiting_clients(),
            io:format(  "(main) Banker has finished notifying waiting Clients."
                        "~n", []),
            io:format(  "(main) Banker status is now: CashOnHand = ~p,"
                        " ClientProcs = ~p.~n"
                        , [NewBanker#banker.cash_on_hand
                        , NewBanker#banker.client_procs]),
            main(NewBanker);
        {'EXIT', Pid, Reason} ->
            io:format(  "(main) An unexpected exit from process ~p was caught"
                        " with reason: ~p.~n"
                        , [Pid, Reason]),
            main(Banker)
    end.
    
%% compare_clients/2
%% Defines the sorting order for clients. (From least claim to greatest claim.)
%% Arguments:
%%  C1: a Client process
%%  C2: a different Client process
compare_clients(C1, C2) ->
    case is_process_alive(C1) of
        true ->
            io:format(  "(compare_clients) Banker is requesting claim from"
                        " Client ~p.~n", [C1]),
            C1 ! {self(), getclaim},
            receive
                {C1, claim, C1_claim} -> C1_claim
            after 100 ->
                C1_claim = 0
            end,
            io:format(  "(compare_clients) Client ~p has claim ~p.~n"
                        , [C1, C1_claim]);
        false ->
            C1_claim = 0
    end,
    case is_process_alive(C2) of
        true ->
            io:format(  "(compare_clients) Banker is requesting claim from"
                        " Client ~p.~n", [C2]),
            C2 ! {self(), getclaim},
            receive
                {C2, claim, C2_claim} -> C2_claim
            after 100 ->
                C2_claim = 0
            end,
            io:format(  "(compare_clients) Client ~p has claim ~p.~n"
                        ,[C2, C2_claim]);
        false ->
            C2_claim = 0
    end,
    C1_claim =< C2_claim.

%% is_safe_state/2
%% Check the list of Client processes and determine if the state is safe.
%% Arguments:
%%  Clients: the list of Client processes.
%%  NUnits: the number of resources requested by a Client.
%% Returns:
%%  true if state is safe, false is not.
is_safe_state([], _) ->
    true;
is_safe_state([CH | CT], CashOnHand) ->
    case is_process_alive(CH) of
        true ->
            io:format(  "(is_safe_state) Banker is requesting claim from Client"
                        " ~p.~n", [CH]),
            CH ! {self(), getclaim},
            receive
                {CH, claim, Claim} -> Claim
            after 100 ->
                Claim = 0
            end,
            io:format("(is_safe_state) Client ~p has claim ~p.~n", [CH, Claim]),
            io:format(  "(is_safe_state) Banker is requesting loan from Client"
                        " ~p.~n", [CH]),
            CH ! {self(), getloan},
            receive
            {CH, loan, Loan} -> Loan
            after 100 ->
                Loan = 0
            end,
            io:format("(is_safe_state) Client ~p has loan ~p.~n", [CH, Loan]);
        false ->
            Claim = 0,
            Loan = 0
    end,
    if
        Claim > CashOnHand ->
            false;
        Claim =< CashOnHand ->
            is_safe_state(CT, CashOnHand + Loan)
    end.

%% notify_waiting_clients/0
%% Go through the mailbox and find all messages from Client procs which are
%% waiting to have their requests processed, and tell them to try_again.
%% Returns:
%%  ok when done.
notify_waiting_clients() ->
    receive
        {Pid, waiting} ->
            io:format(  "(notify_waiting_clients) Banker is notifying waiting"
                        " Client ~p to retry its request.~n"
                        , [Pid]),
            Pid ! try_again,
            notify_waiting_clients()
    after 0 ->
        done
    end.