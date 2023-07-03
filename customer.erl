-module(customer).
-export([start_customer/4]).

%-------------------------------------------------------------------------------
% start_customer/4
%-------------------------------------------------------------------------------

%% Starts the customer process.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - CustomerName: The name of the customer.
%%   - LoanAmount: The total loan amount requested by the customer.
%%   - BankList: The list of banks available for borrowing.
%%
start_customer(Master, CustomerName, LoanAmount, BankList) ->
    timer:sleep(200),
    main_borrow_process(Master, CustomerName, LoanAmount, LoanAmount, BankList).

%-------------------------------------------------------------------------------
% main_borrow_process/5
%-------------------------------------------------------------------------------

%% Performs the main borrowing process for the customer.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - CustomerName: The name of the customer.
%%   - LoanAmount: The total loan amount requested by the customer.
%%   - RemainingAmount: The remaining loan amount to be borrowed.
%%   - BankList: The list of banks available for borrowing.
%%
main_borrow_process(Master, CustomerName, LoanAmount, RemainingAmount, BankList) ->
    case {RemainingAmount, BankList} of
        {0, _} ->
            Master ! {end_customer, CustomerName, LoanAmount, LoanAmount - RemainingAmount};
        {_, []} ->
            Master ! {end_customer, CustomerName, LoanAmount, LoanAmount - RemainingAmount};
        _ ->
            borrow_from_bank(Master, CustomerName, LoanAmount, RemainingAmount, BankList)
    end.

%-------------------------------------------------------------------------------
% borrow_from_bank/5
%-------------------------------------------------------------------------------

%% Borrows from a random bank.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - CustomerName: The name of the customer.
%%   - LoanAmount: The total loan amount requested by the customer.
%%   - RemainingAmount: The remaining loan amount to be borrowed.
%%   - BankList: The list of banks available for borrowing.
%%
borrow_from_bank(Master, CustomerName, LoanAmount, RemainingAmount, BankList) ->
    timer:sleep(timer:seconds(trunc(rand:uniform(100)/ 1000))),
    RequestedBank = lists:nth(rand:uniform(length(BankList)), BankList),
    RequestedAmount = rand:uniform(lists:min([RemainingAmount, 50])),
    BankPID = whereis(RequestedBank),
    BankPID ! {loan, self(), CustomerName, RequestedAmount},
    Master ! {customer_request, CustomerName, RequestedAmount, atom_to_list(RequestedBank)},

    receive
        {approved} ->
            main_borrow_process(Master, CustomerName, LoanAmount, RemainingAmount - RequestedAmount, BankList);
        {denied} ->
            main_borrow_process(Master, CustomerName, LoanAmount, RemainingAmount, lists:delete(RequestedBank, BankList))
        after 4000 ->
            main_borrow_process(Master, CustomerName, LoanAmount, RemainingAmount, lists:delete(RequestedBank, BankList))
    end.

