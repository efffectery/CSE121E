-module(funcs).
-export([average_two_numbers/2,print_parts/1,factorial/1,double/1]).

% Returns the average of two numbers
average_two_numbers(First, Second) ->
    (First + Second) div 2.


% Prints the Head and Tail of a list
print_parts([]) -> [];
print_parts([H|T]) ->
    io:format("Head: ~p~n",[H]),
    io:format("Tail: ~p~n",[T]).

% Returns the factorial of a number
factorial(N)->factorial(N,1).

factorial(N,Accum) when N > 0 -> factorial(N-1,Accum*N);
factorial(N,Accum) when N == 0 -> Accum;
factorial(N,_Accum) when N < 0 -> undefined. % Factorials of negative integers are not defined. Therefore return the atom undefined.

% Double each item in a list of numbers
double([]) -> [];
double([H|T]) ->
    [2 * H | double(T)].



