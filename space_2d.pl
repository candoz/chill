%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         Point representation in 2d space with integer coordinates          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%point(?X, ?Y)
point(X, Y) :-
  (integer(X); var(X)),
  (integer(Y); var(Y)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         Projection of the vector (X0,Y0) -> (X,Y) on N/S/W/E axes          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%north_projection(+point(X0,Y0), +point(X,Y), ?Proj)
north_projection(point(_,Y0), point(_, Y), Proj) :- Proj is Y-Y0.

%south_projection(+point(X0,Y0), +point(X,Y), ?Proj)
south_projection(point(_,Y0), point(_, Y), Proj) :- Proj is Y0-Y.

%east_projection(+point(X0,Y0), +point(X,Y), ?Proj)
east_projection(point(X0,_), point(X,_), Proj) :- Proj is X-X0.

%west_projection(+point(X0,Y0), +point(X,Y), ?Proj)
west_projection(point(X0,_), point(X,_), Proj) :- Proj is X0-X.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%          Move to one particular direction, from (X0,Y0) to (X,Y)           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% one_step_north(+point(X0,Y0), ?point(X,Y))
one_step_north(point(X0,Y0), point(X0,Y)) :- Y is Y0 + 1.

% one_step_south(+point(X0,Y0), ?point(X,Y))
one_step_south(point(X0,Y0), point(X0,Y)) :- Y is Y0 - 1.

% one_step_east(+point(X0,Y0), ?point(X,Y))
one_step_east(point(X0,Y0), point(X,Y0)) :- X is X0 + 1.

% one_step_west(+point(X0,Y0), ?point(X,Y))
one_step_west(point(X0,Y0), point(X,Y0)) :- X is X0 - 1.


% two_steps_north(+point(X0,Y0), ?point(X,Y))
two_steps_north(point(X0,Y0), point(X0,Y)) :- Y is Y0 + 2.

% two_steps_south(+point(X0,Y0), ?point(X,Y))
two_steps_south(point(X0,Y0), point(X0,Y)) :- Y is Y0 - 2.

% two_steps_east(+point(X0,Y0), ?point(X,Y))
two_steps_east(point(X0,Y0), point(X,Y0)) :- X is X0 + 2.

% two_steps_west(+point(X0,Y0), ?point(X,Y))
two_steps_west(point(X0,Y0), point(X,Y0)) :- X is X0 - 2.


% n_steps_north(+point(X0,Y0), ?point(X,Y))
n_steps_north(point(X0,Y0), point(X0,Y)) :- Y is Y0 + N.

% n_steps_south(+point(X0,Y0), ?point(X,Y))
n_steps_south(point(X0,Y0), point(X0,Y)) :- Y is Y0 - N.

% n_steps_east(+point(X0,Y0), ?point(X,Y))
n_steps_east(point(X0,Y0), point(X,Y0)) :- X is X0 + N.

% n_steps_west(+point(X0,Y0), ?point(X,Y))
n_steps_west(point(X0,Y0), point(X,Y0)) :- X is X0 - N.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Miscellaneous                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%adjacent(+point(X0,Y0), ?point(X,Y))
adjacent(point(X0,Y0), point(X,Y)) :-
  (X is X0 - 1; X is X0; X is X0 + 1), 
  (Y is Y0 - 1; Y is Y0; Y is Y0 + 1),
  not (X = X0, Y = Y0).  % comment out this subgoal to include the soultion where (X,Y) is the same point as (X0,Y0)

%% In this (previous) version the 2nd point should have been mandatory instantiated: I found that to be an unnecessary limit. 
%adjacent_old(+point(X1,Y1), +point(X2,Y2))
adjacent_old(point(X1,Y1), point(X2,Y2)) :-
  north_projection(point(X1,Y1), point(X2,Y2), N_Proj), (N_Proj = 0; abs(N_Proj) =:= 1),
  east_projection(point(X1,Y1), point(X2,Y2), E_Proj), (E_Proj = 0; abs(E_Proj) =:= 1),
  (X1 =\= X2; Y1 =\= Y2).  % comment out this subgoal to include the soultion where (X2,Y2) is the same point as (X1,Y1)


%l_pattern(+point(X0,Y0), ?point(X,Y))
l_pattern(point(X0,Y0), point(X,Y)) :-
  (
    (X is X0 + 2; X is X0 - 2),
    (Y is Y0 + 1; Y is Y0 - 1)
  );
  (
    (X is X0 + 1; X is X0 - 1),
    (Y is Y0 + 2; Y is Y0 - 2)
  ).

%% An equivalent (but slightly less efficient) version of l_pattern/2.
%l_pattern_old(+point(X0,Y0), ?point(X,Y))
l_pattern_old(point(X0,Y0), point(X,Y)) :-
  (
    (two_steps_north(point(X0,Y0), point(A,B)); two_steps_south(point(X0,Y0), point(A,B))),
    (one_step_east(point(A,B), point(X,Y)); one_step_west(point(A,B), point(X,Y)))
  );
  (
    (one_step_north(point(X0,Y0), point(A,B)); one_step_south(point(X0,Y0), point(A,B))),
    (two_steps_east(point(A,B), point(X,Y)); two_steps_west(point(A,B), point(X,Y)))
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55 TODO


%in_between(+point(X1,Y1), +point(X2,Y2), -Points)
in_between(point(X1,Y1), point(X2,Y2)) :-
  north_projection(X1, Y1, X2, Y2, N_PROJ), east_projection(X1, Y1, X2, Y2, E_PROJ),
  (
    N_PROJ = E_PROJ  % anti-diagonal
  ).
    