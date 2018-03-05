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

%north(+X0, +Y0, +X, +Y)
north(X0, Y0, X, Y) :- X =:= X0, Y > Y0.

%% Deprecated:

%south(+X0, +Y0, +X, +Y)
south(X0, Y0, X, Y) :- X =:= X0, Y < Y0.

%west(+X0, +Y0, +X, +Y)
west(X0, Y0, X, Y) :- X < X0, Y =:= Y0.

%east(+X0, +Y0, +X, +Y)
east(X0, Y0, X, Y) :- X > X0, Y =:= Y0.

%north_east(+X0, +Y0, +X, +Y)
north_east(X0, Y0, X, Y) :- X > X0, Y > Y0.

%south_east(+X0, +Y0, +X, +Y)
south_east(X0, Y0, X, Y) :- X > X0, Y < Y0.

%south_west(+X0, +Y0, +X, +Y)
south_west(X0, Y0, X, Y) :- X < X0, Y < Y0.

%north_west(+X0, +Y0, +X, +Y)
north_west(X0, Y0, X, Y) :- X < X0, Y > Y0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Simple 2D geometry: relevant lines between (X1, Y1) and (X2, Y2)      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% horizontal(+X1, +Y1, +X2, +Y2)
horizontal(X1, Y1, X2, Y2) :- X1 =\= X2, Y1 =:= Y2.

% vertical(+X1, +Y1, +X2, +Y2)
horizontal(X1, Y1, X2, Y2) :- X1 =\= X2, Y1 =:= Y2.

% horizontal(+X1, +Y1, +X2, +Y2)
horizontal(X1, Y1, X2, Y2) :- X1 =\= X2, Y1 =:= Y2.

% horizontal(+X1, +Y1, +X2, +Y2)
horizontal(X1, Y1, X2, Y2) :- X1 =\= X2, Y1 =:= Y2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Miscellaneous                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%adjacent(+X0, +Y0, ?X, ?Y)
adjacent(X0, Y0, X, Y) :-
    (X is X0 - 1; X is X0; X is X0 + 1), 
    (Y is Y0 - 1; Y is Y0; Y is Y0 + 1),
    not (X = X0, Y = Y0).  % comment out this subgoal to include the soultion where (X1,Y1) is the same point as (X2,Y2)

%deprecated_adjacent(+X1, +Y1, +X2, +Y2)
deprecated_adjacent(X1, Y1, X2, Y2) :-
  north_projection(_, Y1, _, Y2, N_PROJ), (abs(N_PROJ) =:= 0; abs(N_PROJ) =:= 1),
  east_projection(X1, _, X2, _, E_PROJ), (abs(E_PROJ) =:= 0; abs(E_PROJ) =:= 1),
  ((X1 =\= X2); (Y1 =\= Y2)).  % comment out this subgoal to include the soultion where (X1,Y1) is the same point as (X2,Y2)

%l_pattern(+X0, +Y0, ?X, ?Y)
l_pattern(X0, Y0, X, Y) :-
  (
    (north_projection(X0, Y0, A, B, 2); north_projection(X0, Y0, A, B, -2)),
    (east_projection(X0, Y0, A, B, 1); east_projection(X0, Y0, A, B, -1))
  );
  (
    (north_projection(X0, Y0, A, B, 1); north_projection(X0, Y0, A, B, -1)),
    (east_projection(X0, Y0, A, B, 2); east_projection(X0, Y0, A, B, -2))
  ).

%distance(+X1, +Y1, +X2, +Y2, -D)
distance(X1, Y1, X2, Y2, D) :- D is sqrt((X2-X1)^2 + (Y2-Y1)^2).

%%% Relative direction predicates %%% (Ahead, Behind, )

% one_cell_ahead(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  AFTER_X = BEFORE_X,
  ((turn(white), AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_Y is BEFORE_Y-1)).

% one_cell_behind(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_behind(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  AFTER_X = BEFORE_X,
  ((turn(white), AFTER_Y is BEFORE_Y-1) ; (turn(black), AFTER_Y is BEFORE_Y+1)).

% one_cell_right(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_X is BEFORE_X+1) ; (turn(black), AFTER_X is BEFORE_X-1)),
  AFTER_Y = BEFORE_Y.

% one_cell_left(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_X is BEFORE_X-1) ; (turn(black), AFTER_X is BEFORE_X+1)),
  AFTER_Y = BEFORE_Y.

% in_line(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
in_line(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  AFTER_X = BEFORE_X ; AFTER_Y = BEFORE_Y.

% in_diagonal(+BEFORE_X, +BEFORE_Y, +AFTER_X, +AFTER_Y)
in_diagonal(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  ((AFTER_Y - BEFORE_Y) =:= (AFTER_X - BEFORE_X)); % same diagonal
  ((AFTER_Y - BEFORE_Y) =:= (BEFORE_X - AFTER_X)). % same anti-diagonal

path_is_clear(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-  % a path can be a straight or a diagonal line
  (
    in_diagonal(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y),
    north_east_is_clear
  ). %% TODO
 
diagonal_is_free(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  in_diagonal(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y),  % this check can be moved outside
  (
    AFTER_X > BEFORE_X,
    (
      (AFTER_Y > BEFORE_Y, diagonal_is_free(BEFORE_X, BEFORE_Y, AFTER_X-1, AFTER_Y-1))
    )
  ).