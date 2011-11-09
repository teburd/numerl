%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Numerical erlang.

-module(matrix).

-type type() :: int8 | int16 | int32 | int64 | real32
    | real64 | complex32 | complex64.
-type dimensions() :: pos_integer() | tuple(pos_integer()).
-type index() :: pos_integer() | tuple(pos_integer()).
-type matrix() :: {matrix, type(), dimensions(), binary()}.
-type tuple_matrix() :: tuple(number())
    | tuple(tuple(number()))
    | tuple(tuple(tuple(number()))).
-type list_matrix() :: list(number())
    | list(list(number()))
    | list(list(list(number()))).

-export_type([type/0, dimensions/0, index/0, matrix/0, tuple_matrix/0, list_matrix/0]).

-define(NOT_LOADED, not_loaded(?LINE)).

-export([new/1, identity/1, ones/1, zeros/1,
        col/2, row/2, cell/2, add/2, multiply/2,
        transpose/1, invert/1]).

-on_load(init/0).


%% api

%% @doc Create a matrix from a list of lists or a tuple of tuples.
-spec new(list_matrix() | tuple_matrix()) -> matrix().
new(M0) ->
    ?NOT_LOADED.

%% @doc Create a specific kind of matrix from a list of lists or a tuple
%% of tuples.
%% @end
-spec new(type(), list_matrix() | tuple_matrix()) -> matrix().
new(T, M0) ->
    ?NOT_LOADED.

%% @doc Create an identity matrix.
-spec identity(dimensions()) -> matrix().
identity(Dimensions) ->
    ?NOT_LOADED.

%% @doc Create a ones filled matrix.
-spec ones(dimensions()) -> matrix().
ones(Dimensions) ->
    ?NOT_LOADED.

%% @doc Create a zeros filled matrix.
-spec zeros(dimensions()) -> matrix().
zeros(Dimensions) ->
    ?NOT_LOADED.

%% @doc Column of a matrix.
-spec col(matrix(), pos_integer()) -> matrix().
col(M0, I) ->
    ?NOT_LOADED.

%% @doc Row of a matrix.
-spec row(matrix(), pos_integer()) -> matrix().
row(M0, I) ->
    ?NOT_LOADED.

%% @doc Cell value.
-spec cell(matrix(), index()) -> number().
cell(M0, I) ->
    ?NOT_LOADED.

%% @doc Add. Performs a scalar or matrix addition.
-spec add(matrix(), number() | matrix()) -> matrix().
add(M0, M1) ->
    ?NOT_LOADED.

%% @doc Multiply. Performs a scalar or matrix multiply.
-spec multiply(matrix(), number() | matrix()) -> matrix().
multiply(M0, M1) ->
    ?NOT_LOADED.

%% @doc Transpose a matrix.
-spec transpose(matrix()) -> matrix().
transpose(M0) ->
    ?NOT_LOADED.

%% @doc Invert a matrix.
-spec invert(matrix()) -> matrix() | {error, not_invertable}.
invert(M0) ->
    ?NOT_LOADED.

%% @doc Convert a matrix to a tuple of tuples.
-spec to_tuples(matrix()) -> tuple_matrix().
to_tuples(M0) ->
    ?NOT_LOADED.

%% @doc Convert a matrix to a list of lists.
-spec to_lists(matrix()) -> list_matrix().
to_lists(M0) ->
    ?NOT_LOADED.


%% private

%% @doc Load the matrix NIF module. 
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "matrix"), 0).

%% @doc NIF not loaded handler.
not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
