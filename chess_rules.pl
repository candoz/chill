:- include('space_2d.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             General rules                                 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% There are two teams (black and white) that alternate turns.
%next_turn(?CURRENT, ?NEXT)
next_turn(white, black).
next_turn(black, white).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Relative spatial predicates                            %%
%%          White pieces face north, while black piece face south             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%two_cells_ahead(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
two_cells_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  AFTER_X = BEFORE_X,
  ((turn(white), AFTER_Y is BEFORE_Y+2) ; (turn(black), AFTER_Y is BEFORE_Y-2)).

%two_cells_behind(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
two_cells_behind(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  AFTER_X = BEFORE_X,
  ((turn(white), AFTER_Y is BEFORE_Y-2) ; (turn(black), AFTER_Y is BEFORE_Y+2)).

%two_cells_right(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
two_cells_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  ((turn(white), AFTER_X is BEFORE_X+2) ; (turn(black), AFTER_X is BEFORE_X-2)),
  AFTER_Y = BEFORE_Y.

%two_cells_left(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
two_cells_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  ((turn(white), AFTER_X is BEFORE_X-2) ; (turn(black), AFTER_X is BEFORE_X+2)),
  AFTER_Y = BEFORE_Y.

%one_cell_ahead_right(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_ahead_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (turn(white), AFTER_X is BEFORE_X+1, AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_X is BEFORE_X-1, AFTER_Y is BEFORE_Y-1).

%one_cell_ahead_left(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_ahead_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (turn(white), AFTER_X is BEFORE_X-1, AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_X is BEFORE_X+1, AFTER_Y is BEFORE_Y-1).

%one_cell_diagonal_ahead(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_diagonal_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  one_cell_ahead_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) ; one_cell_ahead_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y).




black(PIECE) :- member(PIECE, [bp,br,bn,bb,bq,bk]).
white(PIECE) :- member(PIECE, [bp,br,bn,bb,bq,bk]).
enemy(PIECE) :- 
  (turn(white), black(PIECE));
  (turn(black), white(PIECE)).

pawn_starting_row(Y) :- 
  (turn(white), Y = 2);
  (turn(black), Y = 7).



%%% Legal moves for every piece %%%

% legal_pawn_move(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
legal_pawn_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (% Moving forward to an empty cell (it can move two cells ahead if the pawn is at its starting row and the two cells ahead are both empty):
    (one_cell_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), cell(AFTER_X, AFTER_Y, e));
    (two_cells_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), pawn_starting_row(BEFORE_Y), one_cell_ahead(BEFORE_X, BEFORE_Y, A, B), cell(A, B, e), cell(AFTER_X, AFTER_Y, e))
  );
  (% Moving forward diagonally if there's an enemy piece in the "AFTER" cell:
    one_cell_diagonal_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), cell(AFTER_X, AFTER_Y, SOME_PIECE), enemy(SOME_PIECE)
  ).

% legal_kniht_move(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
legal_knight_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (cell(AFTER_X, AFTER_Y, e) ; (cell(AFTER_X, AFTER_Y, SOME_PIECE) , enemy(SOME_PIECE))),  % the "after" cell is empty or contains an enemy piece
  l_pattern(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y).

% legal_bishop_move(+BEFORE_X, +BEFORE_Y, +AFTER_X, +AFTER_Y)
legal_bishop_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (cell(AFTER_X, AFTER_Y, e) ; (cell(AFTER_X, AFTER_Y, SOME_PIECE) , enemy(SOME_PIECE))),  % the "after" cell is empty or contains an enemy piece
  in_diagonal(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y),





% Pawn promotion rules
%%promote()



%%% For debugging purpose and old tests %%%

turn(white).

cell(1, 8, br). cell(2, 8, bn). cell(3, 8, bb). cell(4, 8, bq). cell(5, 8, bk). cell(6, 8, bb). cell(7, 8, bn). cell(8, 8, br).
cell(1, 7, bp). cell(2, 7, bp). cell(3, 7, bp). cell(4, 7, bp). cell(5, 7, bp). cell(6, 7, bp). cell(7, 7, bp). cell(8, 7, bp).
cell(1, 6, e) . cell(2, 6, e) . cell(3, 6, e) . cell(4, 6, e) . cell(5, 6, e) . cell(6, 6, e) . cell(7, 6, e) . cell(8, 6, e) .
cell(1, 5, e) . cell(2, 5, e) . cell(3, 5, e) . cell(4, 5, e) . cell(5, 5, e) . cell(6, 5, e) . cell(7, 5, e) . cell(8, 5, e) .
cell(1, 4, e) . cell(2, 4, e) . cell(3, 4, e) . cell(4, 4, e) . cell(5, 4, e) . cell(6, 4, e) . cell(7, 4, e) . cell(8, 4, e) .
cell(1, 3, e) . cell(2, 3, e) . cell(3, 3, e) . cell(4, 3, e) . cell(5, 3, e) . cell(6, 3, e) . cell(7, 3, e) . cell(8, 3, e) .
cell(1, 2, wp). cell(2, 2, wp). cell(3, 2, wp). cell(4, 2, wp). cell(5, 2, wp). cell(6, 2, wp). cell(7, 2, wp). cell(8, 2, wp).
cell(1, 1, wr). cell(2, 1, wn). cell(3, 1, wb). cell(4, 1, wq). cell(5, 1, wk). cell(6, 1, wb). cell(7, 1, wn). cell(8, 1, wr).

% memberchk(+Term, ?List) -> used just to check if an element is in a list, famous alternative to member(?Term, ?List). 
memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]):- memberchk(X,T).
