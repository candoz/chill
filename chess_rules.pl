:- include('space_2d.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          General chess rules                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% There are two teams (black and white) that alternate turns.
%next_turn(?CURRENT, ?NEXT)
next_turn(white, black).
next_turn(black, white).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Pieces and teams                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pawn(wp). pawn(bp).      % white and black Pawns
rook(wr). rook(br).      % white and black Rooks
knight(wn). knight(bn).  % white and black kNights
bishop(wb). bishop(bb).  % white and black Bishops
queen(wq). queen(bq).    % white and black Queens
king(wk). king(bk).      % white and balck Kings


%white(?Piece)
white(Piece) :- member(Piece, [wp,wr,wn,wb,wq,wk]).

%black(?Piece)
black(Piece) :- member(Piece, [bp,br,bn,bb,bq,bk]).


%enemies(+A, +B)
enemies(A, B) :- white(A), black(B).
enemies(A, B) :- black(A), white(B).

%allies(+A, +B)
allies(A, B) :- white(A), white(B).
allies(A, B) :- black(A), black(B).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      Legal moves (for every piece)                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%legal_pawn_move(+P0, ?P))
legal_pawn_move(P0, P) :-
  cell(P0, A), cell(P, B),
  pawn(A),
  (%% A pawn can move one step forward to an empty cell.
    one_step_ahead(P0, P),
    B = e
  );
  (%% A pawn can move two cells ahead if the pawn is at its starting row and those two cells are both empty.
    starting_row(A, Y0),
    one_step_ahead(P0, P1), cell(P1, e),
    B = e
  );
  (%% A pawn can move one cell diagonally ahead if there's an enemy piece in the cell of arrival.
    one_step_diagonal_ahead(X0, Y0, X, Y),
    enemies(A, B)
  ).


%legal_kniht_move(+X0, +Y0, ?X, ?Y))
legal_knight_move(X0, Y0, X, Y) :-  %% A knight can move with an "L" pattern to an empty cell or a cell containing an enemy piece.
  cell(X0, Y0, A), cell(X, Y, B),
  knight(A),
  l_pattern(X0, Y0, X, Y),
  (cell(X, Y, e); enemies(A, B)).


%legal_bishop_move(+X0, +Y0, ?X, ?Y)
legal_bishop_move(X0, Y0, X, Y) :-
  cell(X0, Y0, A), cell(X, Y, B),
  bishop(A),
  (cell(X, Y, e); enemies(A, B)),
  aligned_diagonally(point(X0, Y0), point(X, Y)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      Relevant chess facts and rules                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%must_promote(+Piece, +P)
must_promote() :-  % a pawn must promote when reaching the last row.
  pawn(Piece),
  last_row(Point).

  
%legal_promotion(+Piece, +To_piece)
legal_promotion(Piece, To_piece) :-  % a pawn can promote to a queen, a knight, a rook or a bishop of the same team
  allies(Piece, To_piece),
  (rook(To_piece); knight(To_piece); bishop(To_piece); queen(To_piece)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                Handy spatial predicates on a chess board                   %%
%%         (white pieces face north, while black piece face south)            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ahead_projection(cell(X0,Y0,_), cell(X,Y,_), Proj) :- turn(white), north_projection(point(X0,Y0), point(X,Y), Proj).




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

empty_cells([]).
empty_cells([cell(_, _, e) | Tail]) :- empty_cells(Tail).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             D E B U G G I N G   (and old stuff)            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

turn(white).

cell(point(1,8), br). cell(point(2,8), bn). cell(point(3,8), bb). cell(point(4,8), bq). cell(point(5,8), bk). cell(point(6,8), bb). cell(point(7,8), bn). cell(point(8,8), br).
cell(point(1,7), bp). cell(point(2,7), bp). cell(point(3,7), bp). cell(point(4,7), bp). cell(point(5,7), bp). cell(point(6,7), bp). cell(point(7,7), bp). cell(point(8,7), bp).
cell(point(1,6), e) . cell(point(2,6), e) . cell(point(3,6), e) . cell(point(4,6), e) . cell(point(5,6), e) . cell(point(6,6), e) . cell(point(7,6), e) . cell(point(8,6), e) .
cell(point(1,5), e) . cell(point(2,5), e) . cell(point(3,5), e) . cell(point(4,5), e) . cell(point(5,5), e) . cell(point(6,5), e) . cell(point(7,5), e) . cell(point(8,5), e) .
cell(point(1,4), e) . cell(point(2,4), e) . cell(point(3,4), e) . cell(point(4,4), e) . cell(point(5,4), e) . cell(point(6,4), e) . cell(point(7,4), e) . cell(point(8,4), e) .
cell(point(1,3), e) . cell(point(2,3), e) . cell(point(3,3), e) . cell(point(4,3), e) . cell(point(5,3), e) . cell(point(6,3), e) . cell(point(7,3), e) . cell(point(8,3), e) .
cell(point(1,2), wp). cell(point(2,2), wp). cell(point(3,2), wp). cell(point(4,2), wp). cell(point(5,2), wp). cell(point(6,2), wp). cell(point(7,2), wp). cell(point(8,2), wp).
cell(point(1,1), wr). cell(point(2,1), wn). cell(point(3,1), wb). cell(point(4,1), wq). cell(point(5,1), wk). cell(point(6,1), wb). cell(point(7,1), wn). cell(point(8,1), wr).

% memberchk(+Term, ?List) -> used just to check if an element is in a list, famous alternative to member(?Term, ?List). 
memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]):- memberchk(X,T).
