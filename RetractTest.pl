% turn(?COLOR)
turn(white).

% next_turn(?THIS, ?NEXT)
next_turn(white, black).
next_turn(black, white).

% change_turn
change_turn :-
  next_turn(THIS, NEXT),
  retract(turn(THIS)),
  assert(turn(NEXT)),
  !.  % red cut!
%%% old, alternative implementation without next_turn/2 %%% 
%change_turn :- retract(turn(white)), assert(turn(black)), !.  % red cut
%change_turn :- retract(turn(black)), assert(turn(white)), !.  % green cut

cell(1, 8, br). cell(2, 8, bn). cell(3, 8, bb). cell(4, 8, bq). cell(5, 8, bk). cell(6, 8, bb). cell(7, 8, bn). cell(8, 8, br). % 8
cell(1, 7, bp). cell(2, 7, bp). cell(3, 7, bp). cell(4, 7, bp). cell(5, 7, bp). cell(6, 7, bp). cell(7, 7, bp). cell(8, 7, bp). % 7
cell(1, 6, e) . cell(2, 6, e) . cell(3, 6, e) . cell(4, 6, e) . cell(5, 6, e) . cell(6, 6, e) . cell(7, 6, e) . cell(8, 6, e) . % 6  (Empty row)
cell(1, 5, e) . cell(2, 5, e) . cell(3, 5, e) . cell(4, 5, e) . cell(5, 5, e) . cell(6, 5, e) . cell(7, 5, e) . cell(8, 5, e) . % 5  (Empty row)
cell(1, 4, e) . cell(2, 4, e) . cell(3, 4, e) . cell(4, 4, e) . cell(5, 4, e) . cell(6, 4, e) . cell(7, 4, e) . cell(8, 4, e) . % 4  (Empty row)
cell(1, 3, e) . cell(2, 3, e) . cell(3, 3, e) . cell(4, 3, e) . cell(5, 3, e) . cell(6, 3, e) . cell(7, 3, e) . cell(8, 3, e) . % 3  (Empty row)
cell(1, 2, wp). cell(2, 2, wp). cell(3, 2, wp). cell(4, 2, wp). cell(5, 2, wp). cell(6, 2, wp). cell(7, 2, wp). cell(8, 2, wp). % 2
cell(1, 1, wr). cell(2, 1, wn). cell(3, 1, wb). cell(4, 1, wq). cell(5, 1, wk). cell(6, 1, wb). cell(7, 1, wn). cell(8, 1, wr). % 1

contiguous_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  AFTER_X is BEFORE_X,
  ((turn(white), AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_Y is BEFORE_Y-1)).

contiguous_behind(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_Y is BEFORE_Y-1) ; (turn(black), AFTER_Y is BEFORE_Y+1)),
  AFTER_X is BEFORE_X.

contiguous_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_X is BEFORE_X+1) ; (turn(black), AFTER_X is BEFORE_X-1)),
  AFTER_Y is BEFORE_Y.

contiguous_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_X is BEFORE_X-1) ; (turn(black), AFTER_X is BEFORE_X+1)),
  AFTER_Y is BEFORE_Y.

two_cells_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_Y is BEFORE_Y+2) ; (turn(black), AFTER_Y is BEFORE_Y-2)),
  AFTER_X is BEFORE_X.

two_cells_behind(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  ((turn(white), AFTER_Y is BEFORE_Y-2) ; (turn(black), AFTER_Y is BEFORE_Y+2)),
  AFTER_X is BEFORE_X.

two_cells_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  ((turn(white), AFTER_X is BEFORE_X+2) ; (turn(black), AFTER_X is BEFORE_X-2)),
  AFTER_Y is BEFORE_Y.

two_cells_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  ((turn(white), AFTER_X is BEFORE_X-2) ; (turn(black), AFTER_X is BEFORE_X+2)),
  AFTER_Y is BEFORE_Y.

contiguous_diagonal_ahead_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (turn(white), AFTER_X is BEFORE_X+1, AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_X is BEFORE_X-1, AFTER_Y is BEFORE_Y-1).

contiguous_diagonal_ahead_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (turn(white), AFTER_X is BEFORE_X-1, AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_X is BEFORE_X+1, AFTER_Y is BEFORE_Y-1).

contiguous_diagonal_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  contiguous_diagonal_ahead_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) ; contiguous_diagonal_ahead_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y).


% Describes the typical "L-movement" that only a knight can do.
%el_movement(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-

%memberchk(+Term, ?List) -> used just to check if an element is in a list, famous alternative to member(?Term, ?List). 
memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]):- memberchk(X,T).

black(PIECE) :- memberchk(PIECE, [bp,br,bn,bb,bq,bk]).
white(PIECE) :- memberchk(PIECE, [bp,br,bn,bb,bq,bk]).
enemy(PIECE) :- 
  (turn(white), black(PIECE));
  (turn(black), white(PIECE)).

pawn_starting_row(Y) :- 
  (turn(white), Y = 2);
  (turn(black), Y = 7).

move_for_real(PIECE, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  retract(cell(BEFORE_X, BEFORE_Y, PIECE)), assert(cell(BEFORE_X, BEFORE_Y, e)),
  retract(cell(AFTER_X, AFTER_Y, _)), assert(cell(AFTER_X, AFTER_Y, PIECE)).
  
legal_pawn_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  % Moving forward to an empty cell (it can move two cells ahead if the pawn is at its starting row and the two cells ahead are both empty):
  (
    (contiguous_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), cell(AFTER_X, AFTER_Y, e));
    (two_cells_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), pawn_starting_row(BEFORE_Y), contiguous_ahead(BEFORE_X, BEFORE_Y, CONTIGUOUS_AHEAD_X, CONTIGUOUS_AHEAD_Y), cell(CONTIGUOUS_AHEAD_X, CONTIGUOUS_AHEAD_Y, e), cell(AFTER_X, AFTER_Y, e))
  );
  % Moving forward diagonally if there's an enemy piece in the "AFTER" cell:
  (
    contiguous_diagonal_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), cell(AFTER_X, AFTER_Y, SOME_PIECE), enemy(SOME_PIECE),
    retract(cell(BEFORE_X, BEFORE_Y, PAWN)), assert(cell(BEFORE_X, BEFORE_Y, e)),
    retract(cell(AFTER_X, AFTER_Y, SOME_PIECE)), assert(cell(AFTER_X, AFTER_Y, PAWN))
  ).

move_knight(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  

move(PIECE, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (
    turn(white),
    (
      (PIECE = wp, cell(BEFORE_X, BEFORE_Y, wp), legal_pawn_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = wn, cell(BEFORE_X, BEFORE_Y, wn), legal_knight_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y))
    ),
    move_for_real(PIECE, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y),
    change_turn,
    !
  );
  (
    turn(black), PIECE = bp, cell(BEFORE_X, BEFORE_Y, bp), move_pawn(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y),
    retract(turn(black)), assert(turn(white))
  ).