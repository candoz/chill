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


%%% Generic, reusable direction predicates %%%

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

% one_cell_around(+BEFORE_X, +BEFORE_Y, +AFTER_X, +AFTER_Y)
one_cell_around(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (-1 =< AFTER_X - BEFORE_X , 1 >= AFTER_X - BEFORE_X),
  (-1 =< AFTER_Y - BEFORE_Y , 1 >= AFTER_Y - BEFORE_Y),
  (in_line(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) ; in_diagonal(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y)).


%%% Ad-hoc direction predicates %%%

% two_cells_ahead(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
two_cells_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  AFTER_X = BEFORE_X,
  ((turn(white), AFTER_Y is BEFORE_Y+2) ; (turn(black), AFTER_Y is BEFORE_Y-2)).

% two_cells_behind(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
two_cells_behind(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  AFTER_X = BEFORE_X,
  ((turn(white), AFTER_Y is BEFORE_Y-2) ; (turn(black), AFTER_Y is BEFORE_Y+2)).

% two_cells_right(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
two_cells_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  ((turn(white), AFTER_X is BEFORE_X+2) ; (turn(black), AFTER_X is BEFORE_X-2)),
  AFTER_Y = BEFORE_Y.

% two_cells_left(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
two_cells_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  ((turn(white), AFTER_X is BEFORE_X-2) ; (turn(black), AFTER_X is BEFORE_X+2)),
  AFTER_Y = BEFORE_Y.

% one_cell_ahead_right(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_ahead_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (turn(white), AFTER_X is BEFORE_X+1, AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_X is BEFORE_X-1, AFTER_Y is BEFORE_Y-1).

% one_cell_ahead_left(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_ahead_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (turn(white), AFTER_X is BEFORE_X-1, AFTER_Y is BEFORE_Y+1) ; (turn(black), AFTER_X is BEFORE_X+1, AFTER_Y is BEFORE_Y-1).

% one_cell_diagonal_ahead(+BEFORE_X, +BEFORE_Y, ?AFTER_X, ?AFTER_Y)
one_cell_diagonal_ahead(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  one_cell_ahead_right(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) ; one_cell_ahead_left(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y).

% l_pattern(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y)
l_pattern(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (
    two_cells_ahead(BEFORE_X, BEFORE_Y, A, B),
    (one_cell_right(A, B, AFTER_X, AFTER_Y); one_cell_left(A, B, AFTER_X, AFTER_Y))
  );
  (
    two_cells_behind(BEFORE_X, BEFORE_Y, A, B),
    (one_cell_right(A, B, AFTER_X, AFTER_Y); one_cell_left(A, B, AFTER_X, AFTER_Y))
  );
  (
    two_cells_right(BEFORE_X, BEFORE_Y, A, B),
    (one_cell_ahead(A, B, AFTER_X, AFTER_Y); one_cell_behind(A, B, AFTER_X, AFTER_Y))
  );
  (
    two_cells_left(BEFORE_X, BEFORE_Y, A, B),
    (one_cell_ahead(A, B, AFTER_X, AFTER_Y); one_cell_behind(A, B, AFTER_X, AFTER_Y))
  ).


% memberchk(+Term, ?List) -> used just to check if an element is in a list, famous alternative to member(?Term, ?List). 
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

lets_move(PIECE, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  retract(cell(BEFORE_X, BEFORE_Y, PIECE)), assert(cell(BEFORE_X, BEFORE_Y, e)),
  retract(cell(AFTER_X, AFTER_Y, _)), assert(cell(AFTER_X, AFTER_Y, PIECE)).


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
  l_pattern(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y), 
  (
    cell(AFTER_X, AFTER_Y, e);
    (cell(AFTER_X, AFTER_Y, SOME_PIECE), enemy(SOME_PIECE))
  ).

% move(+PIECE, +BEFORE_X, +BEFORE_Y, +AFTER_X, +AFTER_Y)
move(PIECE, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  (
    turn(white),
    (
      (PIECE = wp, cell(BEFORE_X, BEFORE_Y, wp), legal_pawn_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = wn, cell(BEFORE_X, BEFORE_Y, wn), legal_knight_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = wb, cell(BEFORE_X, BEFORE_Y, wb), legal_bishop_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = wr, cell(BEFORE_X, BEFORE_Y, wr), legal_rook_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = wq, cell(BEFORE_X, BEFORE_Y, wq), legal_queen_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = wk, cell(BEFORE_X, BEFORE_Y, wk), legal_king_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y))
    ),
    lets_move(PIECE, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y),
    change_turn,
    !  % is this necessary? there already is a cut inside "change turn"...
  );
  (
    turn(black),
    (
      (PIECE = bp, cell(BEFORE_X, BEFORE_Y, bp), legal_pawn_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = bn, cell(BEFORE_X, BEFORE_Y, bn), legal_knight_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = bb, cell(BEFORE_X, BEFORE_Y, bb), legal_bishop_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = br, cell(BEFORE_X, BEFORE_Y, br), legal_rook_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = bq, cell(BEFORE_X, BEFORE_Y, bq), legal_queen_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y));
      (PIECE = bk, cell(BEFORE_X, BEFORE_Y, bk), legal_king_move(BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y))
    ),
    lets_move(PIECE, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y),
    change_turn,
    !  % is this necessary? there already is a cut inside "change turn"...
  ).
