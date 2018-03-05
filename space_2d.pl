%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         Projection of the vector (X0,Y0) -> (X,Y) on N/S/W/E axes          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%north_projection(+X0, +Y0, +X, +Y, ?PROJ)
north_projection(_, Y0, _, Y, PROJ) :- PROJ is Y - Y0.

%south_projection(+X0, +Y0, +X, +Y, ?PROJ)
south_projection(_, Y0, _, Y, PROJ) :- PROJ is Y0 - Y.

%east_projection(+X0, +Y0, +X, +Y, ?PROJ)
east_projection(X0, _, X, _, PROJ) :- PROJ is X - X0.

%west_projection(+X0, +Y0, +X, +Y, ?PROJ)
west_projection(X0, _, X, _, PROJ) :- PROJ is X0 - X.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%          Move in one particular direction from (X0,Y0) to (X,Y)            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% one_step_north(+X0, +Y0, ?X, ?Y)
one_step_north(X0, Y0, X, Y) :- X = X0, Y is Y0 + 1.

% one_step_south(+X0, +Y0, ?X, ?Y)
one_step_south(X0, Y0, X, Y) :- X = X0, Y is Y0 - 1.

% one_step_east(+X0, +Y0, ?X, ?Y)
one_step_east(X0, Y0, X, Y) :- X is X0 + 1, Y = Y0.

% one_step_west(+X0, +Y0, ?X, ?Y)
one_step_west(X0, Y0, X, Y) :- X is X0 - 1, Y = Y0.


% two_steps_north(+X0, +Y0, ?X, ?Y)
two_steps_north(X0, Y0, X, Y) :- X = X0, Y is Y0 + 2.

% two_steps_south(+X0, +Y0, ?X, ?Y)
two_steps_south(X0, Y0, X, Y) :- X = X0, Y is Y0 - 2.

% two_steps_east(+X0, +Y0, ?X, ?Y)
two_steps_east(X0, Y0, X, Y) :- X is X0 + 2, Y = Y0.

% two_steps_west(+X0, +Y0, ?X, ?Y)
two_steps_west(X0, Y0, X, Y) :- X is X0 - 2, Y = Y0.


% n_steps_north(+N, +X0, +Y0, ?X, ?Y)
n_steps_north(X0, Y0, X, Y) :- X = X0, Y is Y0 + N.

% n_steps_south(+N, +X0, +Y0, ?X, ?Y)
n_steps_south(X0, Y0, X, Y) :- X = X0, Y is Y0 - N.

% n_steps_east(+N, +X0, +Y0, ?X, ?Y)
n_steps_east(X0, Y0, X, Y) :- X is X0 + N, Y = Y0.

% n_steps_west(+N, +X0, +Y0, ?X, ?Y)
n_steps_west(X0, Y0, X, Y) :- X is X0 - N, Y = Y0.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Miscellaneous                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%adjacent(+X0, +Y0, ?X, ?Y)
adjacent(X0, Y0, X, Y) :-
  (X is X0 - 1; X is X0; X is X0 + 1), 
  (Y is Y0 - 1; Y is Y0; Y is Y0 + 1),
  not (X = X0, Y = Y0).  % comment out this subgoal to include the soultion where (X,Y) is the same point as (X0,Y0)

%% In this previous version the 3rd and 4th parameter should have been mandatory instantiated. I found that to be an unnecessary limit. 
%adjacent_old(+X1, +Y1, +X2, +Y2)
adjacent_old(X1, Y1, X2, Y2) :-
  north_projection(_, Y1, _, Y2, N_PROJ), (abs(N_PROJ) =:= 0; abs(N_PROJ) =:= 1),
  east_projection(X1, _, X2, _, E_PROJ), (abs(E_PROJ) =:= 0; abs(E_PROJ) =:= 1),
  ((X1 =\= X2); (Y1 =\= Y2)).  % comment out this subgoal to include the soultion where (X2,Y2) is the same point as (X1,Y1)


%l_pattern(+X0, +Y0, ?X, ?Y)
l_pattern(X0, Y0, X, Y) :-
  (
    (X is X0 + 2; X is X0 - 2),
    (Y is Y0 + 1; Y is Y0 - 1)
  );
  (
    (X is X0 + 1; X is X0 - 1),
    (Y is Y0 + 2; Y is Y0 - 2)
  ).

%% Equivalent but slightly less efficient version of l_pattern/4.
%l_pattern_old(+X0, +Y0, ?X, ?Y)
l_pattern_old(X0, Y0, X, Y) :-
  (
    (two_steps_north(X0, Y0, A, B); two_steps_south(X0, Y0, A, B)),
    (one_step_east(A, B, X, Y); one_step_west(A, B, X, Y))
  );
  (
    (one_step_north(X0, Y0, A, B); one_step_south(X0, Y0, A, B)),
    (two_steps_east(A, B, X, Y); two_steps_west(A, B, X, Y))
  ).


%in_between(+X1, +Y1, +X2, +Y2, -X, -Y)
in_between(X1, Y1, X2, Y2) :-
  