-module(money).
-export([start/1]).

%-------------------------------------------------------------------------------
% start/1
%-------------------------------------------------------------------------------

%% Entry point of the module.
%%
%% Parameters:
%%   - Args: A list of arguments passed to the function.
%%            Args[1]: CustomerFile - File containing customer information.
%%            Args[2]: BankFile - File containing bank information.
%%
%% Returns:
%%   - 'ok' if the process completes successfully.
%%
%% 
start(Args) ->
  CustomerFile = lists:nth(1, Args),
  BankFile = lists:nth(2, Args),

  {ok, CustomerData} = file:consult(CustomerFile),
  {ok, BankData} = file:consult(BankFile),

  MasterProcess = self(),
  BankList = [BankName || {BankName, _} <- BankData],
  CustomerList = [CustomerName || {CustomerName, _} <- CustomerData],

  io:fwrite("~n** The financial Market is opening for the day **~n~n"),
  io:fwrite("Starting transaction log...~n~n"),

  start_customer(MasterProcess, BankList, CustomerData),
  start_bank(MasterProcess, BankData),

  get_transaction_logs(length(CustomerList), [], BankList),

  io:fwrite("Banks: ~n"),
  stop_bank(BankList),

  io:fwrite("The financial market is closing for the day...~n"),
  ok.

%-------------------------------------------------------------------------------
% start_customer/4
%-------------------------------------------------------------------------------

%% Starts customer processes.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - BankList: The list of banks available for borrowing.
%%   - CustomerTuples: A list of tuples containing customer information.
%%
%% Returns:
%%   - 'ok' if the process completes successfully.
%%
start_customer(_, _, []) ->
  ok;
start_customer(Master, BankList, [{CustomerName, LoanAmount} | CustomerTuples]) ->
  Customer = atom_to_list(CustomerName),
  register(CustomerName, spawn(customer, start_customer, [Master, Customer, LoanAmount, BankList])),
  start_customer(Master, BankList, CustomerTuples).

%-------------------------------------------------------------------------------
% get_transaction_logs/3
%-------------------------------------------------------------------------------

%% Receives and processes logs.
%%
%% Parameters:
%%   - N: The number of customer processes.
%%   - StoppedCustomers: A list of tuples containing information about stopped customers.
%%   - BankList: The list of banks available for borrowing.
%%
%% Returns:
%%   - 'ok' if the process completes successfully.
%%
get_transaction_logs(N, StoppedCustomers, BankList) ->
  case N of
    0 ->
      generate_report(StoppedCustomers);
  _ ->
      receive
        {customer_request, Customer, LoanAmount, Bank} ->
          io:fwrite("? ~s requests a loan of ~s dollar(s) from the ~s bank ~n", [Customer, integer_to_list(LoanAmount), (Bank)]),
          get_transaction_logs(N, StoppedCustomers, BankList);
        {bank_approve, Bank, Customer, LoanRequest} ->
            io:fwrite("$ The ~s bank approves a loan of ~s dollar(s) to ~s ~n", [Bank, integer_to_list(LoanRequest), Customer]),
            get_transaction_logs(N, StoppedCustomers, BankList);
        {bank_denied, Bank, Customer, LoanRequest} ->
            io:fwrite("$ The ~s bank denies a loan of ~s dollar(s) to ~s ~n", [Bank, integer_to_list(LoanRequest), Customer]),
            get_transaction_logs(N, StoppedCustomers, BankList);
        {end_customer, Customer, Objective, ReceivedAmount} ->
            get_transaction_logs(N - 1, [{Customer, Objective, ReceivedAmount} | StoppedCustomers], BankList);
        {start, Msg} ->
            io:fwrite("~s~n", [Msg]),
            get_transaction_logs(N, StoppedCustomers, BankList)
      end
  end.

%-------------------------------------------------------------------------------
% generate_report/1
%-------------------------------------------------------------------------------

%% Generates a customer report.
%%
%% Parameters:
%%   - Customers: A list of tuples containing customer information.
%%
%% Returns:
%%   - 'ok' if the process completes successfully.
%%
generate_report(Customers) ->
  io:fwrite("~n~n** Banking Report **~n~n"),
  io:fwrite("Customers:~n"),
  lists:foreach(
      fun({CustomerName, CustomerObjective, CustomerReceived}) ->
          io:fwrite("~s: objective ~B, received ~B ~n", [CustomerName, CustomerObjective, CustomerReceived])
      end,
      Customers
  ),
  io:fwrite("----------~n"),
  {ObjectiveTotal, ReceivedTotal} = lists:foldl(
    fun({_, Objective, Received}, {TotalObj, TotalReceived}) ->
        {TotalObj + Objective, TotalReceived + Received}
    end,
    {0, 0},
    Customers
  ),
  io:fwrite("Total: objective ~B, received ~B ~n~n", [ObjectiveTotal, ReceivedTotal]).

%-------------------------------------------------------------------------------
% start_bank/3
%-------------------------------------------------------------------------------

%% Starts bank processes.
%%
%% Parameters:
%%   - Master: The process identifier (PID) of the master process.
%%   - BankTuples: A list of tuples containing bank information.
%%
%% Returns:
%%   - 'ok' if the process completes successfully.
%%
start_bank(_, []) ->
  ok;
start_bank(Master, [{BankName, InitialBalance} | BankTuples]) ->
  Bank = atom_to_list(BankName),
  register(BankName, spawn(bank, start_bank, [Master, Bank, InitialBalance])),
  start_bank(Master, BankTuples).


%-------------------------------------------------------------------------------
% stop_bank/1
%-------------------------------------------------------------------------------

%% Stops banks and generates a bank report.
%%
%% Parameters:
%%   - BankList: The list of banks available for borrowing.
%%
%% Returns:
%%   - 'ok' if the process completes successfully.
%%
stop_bank(BankList) ->
  BankData = lists:map(
    fun(BankName) ->
        BankProcess = whereis(BankName),
        BankProcess ! {shutdown_bank},
        receive
          {end_bank, Bank, OriginalBalance, CurrentBalance} ->
            io:fwrite("~s: original ~B, balance ~B ~n", [Bank, OriginalBalance, CurrentBalance]),
            {OriginalBalance, CurrentBalance}
        end
    end,
    BankList
  ),
  {OriginalTotal, LoanedTotal} = lists:foldl(
    fun({Original, Loaned}, {TotalOriginal, TotalLoaned}) ->
      {TotalOriginal + Original, TotalLoaned + Loaned}
    end,
    {0, 0},
    BankData
  ),
  io:fwrite("----------~n"),
  io:fwrite("Total: original ~B, loaned ~B ~n~n", [OriginalTotal, OriginalTotal - LoanedTotal]).
