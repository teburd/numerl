%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick
%% @doc Matrix Creation and Manipulation Functions.

-module(matrix).

-type dimensions() :: pos_integer() | tuple(pos_integer()).
-type matrix() :: binary().
-type tuple_matrix() :: tuple(number())
    | tuple(tuple(number()))
    | tuple(tuple(tuple(number()))).
-type list_matrix() :: list(number())
    | list(list(number()))
    | list(list(list(number()))).

-export_type([dimensions/0, matrix/0, tuple_matrix/0, list_matrix/0]).

-define(NOT_LOADED, not_loaded(?LINE)).

-export([identity/1, ones/1, zeros/1, multiply/2,
    transpose/1]).

-on_load(init/0).

%% api

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

%% @doc Multiply two matrices.
-spec multiply(matrix(), matrix()) -> matrix().
multiply(M0, M1) ->
    ?NOT_LOADED.

%% @doc Transpose a matrix.
-spec transpose(matrix()) -> matrix().
transpose(M0) ->
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
