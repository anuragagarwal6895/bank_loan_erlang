-module(bank).
-export([start_bank/3]).

%-------------------------------------------------------------------------------
% start_bank/3
%-------------------------------------------------------------------------------

%% Starts the bank process.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - BankName: The name of the bank.
%%   - InitialBalance: The initial balance of the bank.
%%
start_bank(Master, BankName, InitialBalance) ->
    timer:sleep(200),
    run_bank_transactions(Master, BankName, InitialBalance, InitialBalance).

%-------------------------------------------------------------------------------
% handle_loan_request/6
%-------------------------------------------------------------------------------

%% Handles a loan request from a customer and determines whether to approve or deny the loan.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - BankName: The name of the bank.
%%   - CustomerPID: The process identifier (PID) of the customer process.
%%   - CustomerName: The name of the customer.
%%   - LoanRequested: The amount of loan requested by the customer.
%%   - CurrentBalance: The current balance of the bank.
%%
handle_loan_request(Master, BankName, CustomerPID, CustomerName, LoanRequested, CurrentBalance) ->
    case LoanRequested =< CurrentBalance of
        true ->
            approve_loan(Master, BankName, CustomerPID, CustomerName, LoanRequested),
            {approved, CurrentBalance - LoanRequested};
        false ->
            deny_loan(Master, BankName, CustomerPID, CustomerName, LoanRequested),
            denied
    end.

%-------------------------------------------------------------------------------
% run_bank_transactions/4
%-------------------------------------------------------------------------------

%% Runs the bank process and handles customer requests and loans.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - BankName: The name of the bank.
%%   - OriginalBalance: The original balance of the bank.
%%   - CurrentBalance: The current balance of the bank.
%%
run_bank_transactions(Master, BankName, OriginalBalance, CurrentBalance) ->
    receive
        {loan, CustomerPID, CustomerName, LoanRequested} ->
            case handle_loan_request(Master, BankName, CustomerPID, CustomerName, LoanRequested, CurrentBalance) of
                {approved, NewBalance} ->
                    run_bank_transactions(Master, BankName, OriginalBalance, NewBalance);
                denied ->
                    run_bank_transactions(Master, BankName, OriginalBalance, CurrentBalance)
            end;
        {shutdown_bank} ->
            shutdown_bank(Master, BankName, OriginalBalance, CurrentBalance)
    after 4000 ->
        shutdown_bank(Master, BankName, OriginalBalance, CurrentBalance)
    end.

%-------------------------------------------------------------------------------
% deny_loan/5
%-------------------------------------------------------------------------------

%% Notifies the customer that their loan request has been denied.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - BankName: The name of the bank.
%%   - CustomerPID: The process identifier (PID) of the customer process.
%%   - CustomerName: The name of the customer.
%%   - LoanRequested: The amount of loan requested by the customer.
%%
deny_loan(Master, BankName, CustomerPID, CustomerName, LoanRequested) ->
    CustomerPID ! {denied},
    Master ! {bank_denied, BankName, CustomerName, LoanRequested}.

%-------------------------------------------------------------------------------
% approve_loan/5
%-------------------------------------------------------------------------------

%% Notifies the customer that their loan request has been approved.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - BankName: The name of the bank.
%%   - CustomerPID: The process identifier (PID) of the customer process.
%%   - CustomerName: The name of the customer.
%%   - LoanRequested: The amount of loan requested by the customer.
%%
approve_loan(Master, BankName, CustomerPID, CustomerName, LoanRequested) ->
    CustomerPID ! {approved},
    Master ! {bank_approve, BankName, CustomerName, LoanRequested}.

%-------------------------------------------------------------------------------
% shutdown_bank/4
%-------------------------------------------------------------------------------

%% Performs necessary actions when the bank is being shut down.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - BankName: The name of the bank.
%%   - OriginalBalance: The original balance of the bank.
%%   - CurrentBalance: The current balance of the bank.
%%
shutdown_bank(Master, BankName, OriginalBalance, CurrentBalance) ->
    Master ! {end_bank, BankName, OriginalBalance, CurrentBalance},
    ok.