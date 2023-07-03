# bank_loan_erlang
## Bank and Customer Loan Transactions using Erlang

Briefly, your objective is to model a simple banking environment. Specifically, you will be given a small number of customers, each of whom will contact a set of banks to request a number of loans. Eventually, they will either receive all of the money they require or they will terminate without completely meeting their original objective. The application will display information about the various banking transactions before it finishes. That’s it.
DETAILS: So now for the details. To begin, you will need a handful of customers and banks. These will be supplied in a pair of very simple text files - one for customers and one for banks. While Erlang provides many file primitives for processing disk files, the process is not quite as simple as Clojure’s slurp() function. So your two input files will contain records that are already pre- formatted. In other words, they are ready to be read directly into standard Erlang data structures.
 
An example customer file might be:<br>
{jill,450}.<br>
{joe,157}.<br>
{bob,100}.<br>

An example bank file might be:<br>
{rbc,800}.<br>
{bmo,700}.<br>
{ing,200}.<br>

In other words, each file simply contains a set of Erlang tuples. The first element of each tuple is an atom (note that atoms start with a lower-case letter). The atoms will represent the names of the customers and banks respectively (so no string processing is required). You will see that each atom/label is associated with a number. For customers, this is the total funds that they are hoping to obtain. For banks, the number represents their total financial resources that can be used for loans.
To read these files, we use the consult() function in the file module. This will load the contents of either file directly into an Erlang structure (i.e., a list of tuples). Note that NO error checking is required. The text files are guaranteed to contain valid data.

Inside the money.erl source file, you will have the following code at the top of the file: -module(money).
-export([start/1]).
start(Args) ->
     CustomerFile = lists:nth(1, Args),
     BankFile = lists:nth(2, Args),
{ok, CustomerInfo} = file:consult(CustomerFile), {ok, BankInfo} = file:consult(BankFile),
...
Note that the ... at the end simply means that the start function will have more logic after this point. This is just the code to read the data files. In summary, the code above provides the module name for the current source file (money), and it indicates that the module exports a single function called start. The start function will take one parameter which, in this case, is going to be a list of the two command line args: [“c1.txt”, “b1.txt”]. We assign “c1.txt” to CustomerFile and “b1.txt” to BankFile. We then use the consult function in the file module to read the content of each file and bind it to labels. So CustomerInfo will be a list of customer tuples [{jill,450}, {joe,157}, {bob,100}], while BankInfo will be a list of bank tuples [{rbc,800}, {bmo,700}, {ing,200}]. At this point, you have all of your input data and you are ready to proceed.
So your job now is to take this information and create an application that models the banking environment. Because customers and banks are distinct entities in this world, they will be modeled as separate tasks/processes. When the application begins, it will therefore generate a new process for each customer and each bank. Because you do not know how many customers or banks there will be, or even their names, you cannot “hard code” this phase of the application.
The customer and bank tasks will then start up and wait for contact. You may want to make each new customer sleep for 200 milliseconds or so, just to make sure that all the bank tasks have been created and are ready to be used - this can be done trivially with an expression like timer:sleep(200). Otherwise, the application may crash if a customer tries to contact a bank that does not yet exist.

The banking mechanism itself works as follows:
1. Each customer wants to borrow the amount listed in the input file. At any one time, however, they can only request a maximum of 50 dollars. When they make a request, they will therefore choose a random dollar amount between 1 and 50 for their current loan.
2. When they make a request, they will also randomly choose one of the banks as the target.
3. Before each request, a customer will wait/sleep a random period between 10 and 100
milliseconds. This is just to ensure that one customer doesn’t take all the money from the
banks at once.
4. So the customer will make the request and wait for a response from the bank. It will not
make another request until it gets a reply about the current request.
5. The bank can accept or reject the request. It will reject the request if the loan would reduce its current financial resources below 0. Otherwise, it grants the loan and notifies the customer.
6. If the loan is granted, the customer will deduct this amount from its total loan objective and then randomly choose a bank (possibly the same one) and make another request. As noted, the loan requests can never be greater than 50 dollars. However, once the customer’s remaining loan amount is less than 50 dollars, then the next loan request cannot be greater than that amount. In other words, the loan request is actually a random number between 1 and [50 or the remaining loan objective, whichever is smaller].
7. If the loan is rejected, however, the customer will remove that bank from its list of potential lenders, and then submit a new request to one of the remaining banks. Just to be completely clear, a customer never sends another (smaller) request to a bank that has denied a previous request.
8. This process continues until customers have either received all of their money or they have no available banks left to contact.
And that’s it.
Of course, we need a way to demonstrate that all of this has worked properly. To begin, it is important to understand that this is a multi-process Erlang program. The “master” process will be the initial process that, in turn, spawns processes for each of the customers and each of the banks. So, in our little example above, there will be 7 processes in total: the master, 3 customers, and 3 banks.

To confirm the validity of the program, we need a series of info messages to be printed to the screen. This will work as follows:
• When a customer requests a loan amount, they will send a message to a bank with the relevant info.
• The customer will also send a message to the master process, providing the details of the loan request.
• The master process will then print this info to the screen so that the user (you or the grader) can see the loan requests as they happen.
• When a bank responds to a loan request, it will send the approval/denial to the relevant customer.
• The bank will also send this information to the master process.
• The master process will then display the bank’s response to the screen.
In effect, the loan request/reponse info will create a real-time transaction log as the program is running.

## IMPORTANT:

The “master” process is the only process that should display anything to the screen. Customer and Bank processes NEVER do any I/O themselves. In production applications this would also be true since (1) concurrent I/O from multiple tasks would get interleaved together, creating an unreadable mess, and (2) large applications would often use networked/distributed nodes that would not even use the same console/screen. The graders will check your source code to ensure that all printing is done from the master.
Of course, the transaction log is very detailed and it can be hard to determine if the banking transaction are being done properly. So once all of the customer/bank transactions are finished, the master process will display a final report that summarizes the transaction results in a simple way.


Let’s review the output. The first couple of lines simply indicate that the program is starting and we are about to see the transaction log. This may remain on the screen for a half second or so as the customers sleep for a moment to let the banks gets started.
Once the transactions start, they appear very quickly. The first two messages show that sam is asking for 11 dollars from the apple bank, and then the apple bank approves this loan. Loan requests are prefixed with a ? symbol, while bank replies are prefixed with $. You should be able to go through the complete transaction log and find matching request/reply pairs. Note that a bank may sometimes reply to several customer requests at once so we may sometimes see several $ messages in a row. This is perfectly OK.
Once all of the customers have either received their full loan amount or have no more banks from which to borrow, the master process will display the final report. Here we see a list of customers, showing their initial loan objectives, along with the amount they actually received. In addition, we
see the total of all loan objectives, along with a total for all money received. Note that the total received can NEVER be more than the total objectives.
Information for the banks is then provided. For each bank we see their original resources, along with their final balance. In many cases, these balances will be close to 0, indicating that the bank has essentially run out of money. Note that in these cases the balance does not have to be exactly 0, since a customer will stop contacting a bank as soon as that bank denies a loan request (e.g., the customer asks for $20 but the bank only has $4). The final output line shows the total original resources, along with the total actually loaned. Again, the amount loaned cannot be greater than the original resources.
IMPORTANT: All of the summary information must be consistent. Most importantly, the total loaned by the banks must be exactly the same as the total received by the customers. If it’s not, your application is broken.
One final thing. Because of the random pause before each loan request, as well as the random loan amounts in the loan requests, the numbers in the final report should be slightly different each time you run the program, even for the same input files.

## DELIVERABLES:

Your Erlang submission will have just 3 source files. The “main” file will be called money.erl and will correspond to the master process. The second file will be called customer.erl and will include the code associated with the “customer” processes. The final file, of course, will be bank.erl and will represent the bank processes. Module names will be identical to the file names (minus the .erl extension). Do not include any data files, as the markers will provide their own.
