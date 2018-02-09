turn(white).

% "adj" for "adjacent"
% "diag" for "diagonal"
adjacent_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_Y is BEFORE_Y-1)),
  AFTER_X is BEFORE_X.
adjacent_behind(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_Y is BEFORE_Y-1) ; (turn(black), AFTER_Y is BEFORE_Y+1)),
  AFTER_X is BEFORE_X.
adjacent_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_X is BEFORE_X+1) ; (turn(black), AFTER_X is BEFORE_X-1)),
  AFTER_Y is BEFORE_Y.
adjacent_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_X is BEFORE_X-1) ; (turn(black), AFTER_X is BEFORE_X+1)),
  AFTER_Y is BEFORE_Y.
adjacent_diagonal_ahead_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (turn(white), AFTER_X is BEFORE_X+1, AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_X is BEFORE_X-1, AFTER_Y is BEFORE_Y-1).
adjacent_diagonal_ahead_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (turn(white), AFTER_X is BEFORE_X-1, AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_X is BEFORE_X+1, AFTER_Y is BEFORE_Y-1).
adjacent_diagonal_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  adj_diag_ahead_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) ; adj_diag_ahead_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y).
two_cells_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :- 
  ((turn(white), AFTER_Y is BEFORE_Y+2) ; (turn(black), AFTER_Y is BEFORE_Y-2)),
  AFTER_X is BEFORE_X.
  
cell(1, 8, br). cell(2, 8, bn). cell(3, 8, bb). cell(4, 8, bq). cell(5, 8, bk). cell(6, 8, bb). cell(7, 8, bn). cell(8, 8, br). % 8
cell(1, 7, bp). cell(2, 7, bp). cell(3, 7, bp). cell(4, 7, bp). cell(5, 7, bp). cell(6, 7, bp). cell(7, 7, bp). cell(8, 7, bp). % 7
cell(1, 6, e) . cell(2, 6, e) . cell(3, 6, e) . cell(4, 6, e) . cell(5, 6, e) . cell(6, 6, e) . cell(7, 6, e) . cell(8, 6, e) . % 6  (Empty row)
cell(1, 5, e) . cell(2, 5, e) . cell(3, 5, e) . cell(4, 5, e) . cell(5, 5, e) . cell(6, 5, e) . cell(7, 5, e) . cell(8, 5, e) . % 5  (Empty row)
cell(1, 4, e) . cell(2, 4, e) . cell(3, 4, e) . cell(4, 4, e) . cell(5, 4, e) . cell(6, 4, e) . cell(7, 4, e) . cell(8, 4, e) . % 4  (Empty row)
cell(1, 3, e) . cell(2, 3, e) . cell(3, 3, e) . cell(4, 3, e) . cell(5, 3, e) . cell(6, 3, e) . cell(7, 3, e) . cell(8, 3, e) . % 3  (Empty row)
cell(1, 2, wp). cell(2, 2, wp). cell(3, 2, wp). cell(4, 2, wp). cell(5, 2, wp). cell(6, 2, wp). cell(7, 2, wp). cell(8, 2, wp). % 2
cell(1, 1, wr). cell(2, 1, wn). cell(3, 1, wb). cell(4, 1, wq). cell(5, 1, wk). cell(6, 1, wb). cell(7, 1, wn). cell(8, 1, wr). % 1

is_black(PIECE) :- PIECE = (bp; br; bn; bb; bq; bk).
is_white(PIECE) :- PIECE = (wp; wr; wn; wb; wq; wk).
is_enemy(PIECE) :- 
  (turn(white), is_black(PIECE));
  (turn(black), is_white(PIECE)).

move_pawn(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (
    (
      (adjacent_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), cell(AFTER_X, AFTER_Y, e));
      (two_cells_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), adjacent_ahead(BEFORE_X, BEFORE_Y, ADJACENT_AHEAD_X, ADJACENT_AHEAD_Y), cell(ADJACENT_AHEAD_X, ADJACENT_AHEAD_Y, e), cell(AFTER_X, AFTER_Y, e))
    ),
    retract(cell(BEFORE_X, BEFORE_Y, PAWN)), assert(cell(BEFORE_X, BEFORE_Y, e)),
    retract(cell(AFTER_X, AFTER_Y, e)), assert(cell(AFTER_X, AFTER_Y, PAWN)),
    retract(turn(white)), assert(turn(black))
  );
  (
    adjacent_diagonal_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), cell(AFTER_X, AFTER_Y, SOME_PIECE), is_enemy(SOME_PIECE),
    retract(cell(BEFORE_X, BEFORE_Y, PAWN)), assert(cell(BEFORE_X, BEFORE_Y, e)),
    retract(cell(AFTER_X, AFTER_Y, SOME_PIECE)), assert(cell(AFTER_X, AFTER_Y, PAWN)),
    retract(turn(white)), assert(turn(black))
  ).

move(PIECE, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (
    turn(white),
    (PIECE = wp, cell(BEFORE_X, BEFORE_Y, wp), move_pawn(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y))
  );
  (
    turn(black),
    (PIECE = bp, cell(BEFORE_X, BEFORE_Y, bp), move_pawn(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y))
  ).