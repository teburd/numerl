Numerical Erlang
================

Efficient matrix math in erlang in the same spirit as NumPy.

Example Usage 
-------------

An example of the potential future usage of the matrix module in NumErl.

matrix_demo.erl

``` erlang
-module(matrix_demo).

-export([do_it/0]).

do_it() ->
    %% Matrix 3x3 full of ones
    MyMatrix = matrix:ones(3),

    %% Matrix 3x3 identity
    Identity = matrix:identity(3),
   
    %% Transposed ones matrix is in fact equal as expected.
    MyMatrix = matrix:transpose(MyMatrix),

    %% Identity multiplied by MyMatrix gives MyMatrix
    MyMatrix = matrix:multiply(MyMatrix, Identity),

    %% Convert MyMatrix to a tuple of tuples of numbers
    TMatrix = matrix:to_tuples(MyMatrix),

    %% Convert MyMatrix to a list of lists of numbers
    LMatrix = matrix:to_lists(MyMatrix),

    io:format("TMatrix ~p~n", [TMatrix]),
    io:format("LMatrix ~p~n", [LMatrix]),

    ok.
```
