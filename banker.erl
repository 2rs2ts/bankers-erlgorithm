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
-record(banker,
        { capital :: non_neg_integer()
        , cash_on_hand :: non_neg_integer()
        , clients :: list()
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
    register(banker, spawn(fun() -> main(Banker) end)).

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
    receive
        {Pid, status} ->
            Pid ! { Banker#banker.capital
                  , Banker#banker.cash_on_hand
                  , length(Banker#banker.clients
                  };
        {Pid, attach, Limit} ->
            
        {Pid, request, NUnits} ->
            
        {Pid, release, NUnits} ->
            
        _ ->
            io:format("Banker received an unexpected message.~n", [])
    end.