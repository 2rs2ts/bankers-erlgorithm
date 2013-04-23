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
            % could we poll for the claim and loan here?
            % self() ! {banker, getclaim}
            % self() ! {banker, getloan}
            % banker ! {self(), request, NUnits, Claim, Loan}
            % but then we need the others' claims and loans...
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
        {Pid, status} ->
            io:format(  "(main) Banker status was requested. CashOnHand = ~p, 
                        ClientProcs = ~p.~n"
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
            main(NewBanker);
        {_Pid, attach, _Limit} ->
            throw(limit_exceeds_capital);
        {Pid, request, NUnits} ->
            io:format(  "(main) Client ~p requesting ~p resources from Banker.~n"
                        , [Pid, NUnits]),
            %io:format("(main) Banker trying to kill Client ~p!~n", [Pid]),
            %exit(Pid, die_before_request),
            Compare_Clients = fun(C1, C2) -> compare_clients(C1, C2) end,
            io:format("(main) Banker is sorting clients: ~p.~n", [ClientProcs]),
            % differentiate between these checks and safe_state check
            SortedClients = lists:sort(Compare_Clients, ClientProcs),
            io:format("(main) Banker sorted clients: ~p.~n", [SortedClients]),
            io:format("(main) Banker is checking for safe state.~n", []),
            NewBanker = case is_safe_state(SortedClients, CashOnHand) of
                true ->
                    io:format("(main) State is safe.~n", []),
                    polling_done(SortedClients),
                    Pid ! ok,
                    #banker { capital = Capital
                            , cash_on_hand = CashOnHand - NUnits
                            , client_procs = SortedClients
                            };
                false ->
                    io:format("(main) State is not safe.~n", []),
                    polling_done(SortedClients),
                    Pid ! {self(), unsafe}
            end,
            %io:format("(main) Banker trying to kill Client ~p!~n", [Pid]),
            %exit(Pid, die_after_request),
            io:format(  "(main) Banker status is now: CashOnHand = ~p, 
                        ClientProcs = ~p.~n"
                        , [NewBanker#banker.cash_on_hand, NewBanker#banker.client_procs]),
            main(NewBanker);
        {Pid, release, NUnits} ->
            io:format(  "(main) Client ~p releasing ~p resources from Banker.~n"
                        , [Pid, NUnits]),
            %io:format("(main) Banker trying to kill Client ~p!~n", [Pid]),
            %exit(Pid, die_after_release),
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand + NUnits
                                , client_procs = ClientProcs
                                },
            io:format(  "(main) Banker is notifying waiting Clients to try again.~n"
                        , []),
            notify_waiting_clients(),
            io:format(  "(main) Banker has finished notifying waiting Clients.~n", []),
            io:format(  "(main) Banker status is now: CashOnHand = ~p, 
                        ClientProcs = ~p.~n"
                        , [NewBanker#banker.cash_on_hand, NewBanker#banker.client_procs]),
            main(NewBanker);
        {'EXIT', Pid, {finished, Loan}} ->
            io:format(  "(main) Banker reclaims ~p resources from exiting Client ~p.~n"
                        , [Loan, Pid]),
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand + Loan
                                , client_procs = lists:delete(Pid, ClientProcs)
                                },
            io:format(  "(main) Banker status after exit: CashOnHand = ~p, 
                        ClientProcs = ~p.~n"
                        , [NewBanker#banker.cash_on_hand, NewBanker#banker.client_procs]),
            main(NewBanker);
        {'EXIT', Pid, {terminated, Loan}} ->
            io:format(  "(main) Banker reclaims ~p resources from terminated Client ~p.~n"
                        , [Loan, Pid]),
            NewBanker = #banker { capital = Capital
                                , cash_on_hand = CashOnHand + Loan
                                , client_procs = lists:delete(Pid, ClientProcs)
                                },
            io:format(  "(main) Banker status after exit: CashOnHand = ~p, 
                        ClientProcs = ~p.~n"
                        , [NewBanker#banker.cash_on_hand, NewBanker#banker.client_procs]),
            main(NewBanker);
        {'EXIT', Pid, Reason} ->
            io:format(  "(main) An unexpected exit from process ~p was caught with reason: ~p.~n"
                        , [Pid, Reason]),
            main(Banker)
    end.
    
%% compare_clients/2
%% Defines the sorting order for clients. (From least claim to greatest claim.)
%% Arguments:
%%  C1: a Client record
%%  C2: a different Client record
compare_clients(C1, C2) ->
    % These requests are made during a Client request()
    % We should answer these requests during our request but what if the other
    % clients aren't requesting at that moment?
    % How many comparisons are done in lists:sort()?
    io:format("(compare_clients) Banker is requesting claim from Client ~p.~n", [C1]),
    C1 ! {self(), getclaim},
    receive
        % This message is 100% vital to the algorithm and we have to block here.
        % We have to check to see if the process is dead though.
        {C1, claim, C1_claim} -> C1_claim
    end,
    io:format("(compare_clients) Client ~p has claim ~p.~n", [C1, C1_claim]),
    io:format("(compare_clients) Banker is requesting claim from Client ~p.~n", [C2]),
    C2 ! {self(), getclaim},
    receive
        % This message is 100% vital to the algorithm and we have to block here.
        % We have to check to see if the process is dead though.
        {C2, claim, C2_claim} -> C2_claim
    end,
    io:format("(compare_clients) Client ~p has claim ~p.~n", [C2, C2_claim]),
    C1_claim =< C2_claim.

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
    % These requests are made during a Client request().
    io:format("(is_safe_state) Banker is requesting claim from Client ~p.~n", [CH]),
    % Need to time out in case the process died?
    CH ! {self(), getclaim},
    receive
        % This message is 100% vital to the algorithm and we have to block here.
        % We have to check to see if the process is dead though.
        {CH, claim, Claim} -> Claim
    end,
    io:format("(is_safe_state) Client ~p has claim ~p.~n", [CH, Claim]),
    io:format("(is_safe_state) Banker is requesting loan from Client ~p.~n", [CH]),
    CH ! {self(), getloan},
    receive
        % This message is 100% vital to the algorithm and we have to block here.
        % We have to check to see if the process is dead though.
        {CH, loan, Loan} -> Loan
    end,
    io:format("(is_safe_state) Client ~p has loan ~p.~n", [CH, Loan]),
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
            io:format(  "(notify_waiting_clients) Banker is notifying waiting Client ~p to retry its
                        request.~n"
                        , [Pid]),
            Pid ! try_again,
            notify_waiting_clients()
    end.

%% polling_done/1
%% Go through list of Client procs which have been polled for their state
%% and notify them that they won't be expected to provide their state until
%% a later time.
%% Arguments:
%%  [CH | CT] - a list of Client processes.
polling_done([]) -> done;
polling_done([CH | CT]) ->
    io:format("(polling_done) Notifying Client ~p that polling is done.~n", [CH]),
    CH ! {self(), polling_done},
    polling_done(CT).