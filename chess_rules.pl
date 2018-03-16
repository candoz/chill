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



%legal_pawn_move(+P0, +P))
legal_pawn_move(P0, P) :-
  cell(P0, Piece),
  pawn(Piece),
  (%% A pawn can move one step forward to an empty cell.
    steps_ahead(P0, P, 1), cell(P, e)
  );
  (%% A pawn can move two cells ahead if the pawn is at its starting row and those two cells are both empty.
    starting_row(Piece, Y0),
    steps_ahead(P0, P1, 1), cell(P1, e),
    steps_ahead(P0, P, 2), cell(P, e)
  );
  (%% A pawn can move one cell diagonally ahead if there's an enemy piece in the cell of arrival.
    steps_ahead_right(P0, P, Content),
    enemies(Piece, Content)
  ).


%legal_kniht_move(+P0, +P)
legal_knight_move(P0, P) :-  %% A knight can move with an "L" pattern to an empty cell or to a cell containing an enemy piece.
  cell(P0, Piece),
  knight(Piece),
  l_pattern(P0, P),
  cell(P, Content),
  (Content = e; enemies(Piece, Content)).


%legal_bishop_move(+P0, ?P)
legal_bishop_move(P0, P) :-  %% A bishop can move diagonlly to an empty cell or to a cell containing an enemy piece. 
  cell(P0, Piece),           %% All the cells in between must be empty.
  bishop(Piece),
  aligned_diagonally(P0, P),
  ((in_between(P0, P, Points), empty_cells(Points)); not(in_between(P0, P, Points))),
  cell(P, Content),
  (Content = e; enemies(Piece, Content)).


%legal_move(+P0, ?P))
legal_move(P0, P) :- legal_pawn_move(P0, P), !.    % green cut
legal_move(P0, P) :- legal_knight_move(P0, P), !,  % green cut
legal_move(P0, P) :- legal_bishop_move(P0, P).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      Relevant chess facts and rules                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%must_promote(+Piece, +P)
must_promote(Piece, P) :-  % Pawns must promote when reaching the last row.
  pawn(Piece),
  last_row(P).

%legal_promotion(+Piece, +To_piece)
legal_promotion(Piece, To_piece) :-  % Promotion can be to a queen, a knight, a rook or a bishop of the same team.
  allies(Piece, To_piece),
  (rook(To_piece); knight(To_piece); bishop(To_piece); queen(To_piece)).


%starting_row(+Piece, -Row)
starting_row(Piece, Row) :-
  white(Piece),
  (%% White pawns start from the 2nd row, all the other white pieces start from the 1st row.
    (pawn(Piece), Row = 2);
    (not(pawn(Piece)), Row = 1)
  ).
starting_row(Piece, Row) :-
  black(Piece),
  (%% Black pawns start from the 7th row, all the other black pieces start from the 8th row.
    (pawn(Piece), Row = 7);
    (not(pawn(Piece)), Row = 8)
  ).

%last_row(?Row)
last_row(Row) :- turn(white), Row = 8.  %% The 8th row is the last one for the white player.
last_row(Row) :- turn(black), Row = 1.  %% The 1st row is the last one for the black player.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                Handy spatial predicates for a chess board                  %%
%%         (white pieces face north, while black piece face south)            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NB: Remember that, by design, "Steps" > 0.
%%     At least one between "P" and "Steps" must be istantiated.


%steps_ahead(+P0, ?P, ?Steps)
steps_ahead(P0, P, Steps) :- turn(white), steps_north(P0, P, Steps), !.  % green cut
steps_ahead(P0, P, Steps) :- turn(black), steps_south(P0, P, Steps).

%steps_behind(+P0, ?P, ?Steps)
steps_behind(P0, P, Steps) :- turn(white), steps_south(P0, P, Steps), !.  % green cut
steps_behind(P0, P, Steps) :- turn(black), steps_north(P0, P, Steps).

%steps_right(+P0, ?P, ?Steps)
steps_right(P0, P, Steps) :- turn(white), steps_east(P0, P, Steps), !.  % green cut
steps_right(P0, P, Steps) :- turn(black), steps_west(P0, P, Steps).

%steps_left(+P0, ?P, ?Steps)
steps_left(P0, P, Steps) :- turn(white), steps_west(P0, P, Steps), !.  % green cut
steps_left(P0, P, Steps) :- turn(black), steps_east(P0, P, Steps).


%steps_ahead_right(+P0, ?P, ?Steps)
steps_ahead_right(P0, P, Steps) :- turn(white), steps_north_east(P0, P, Steps), !.  % green cut
steps_ahead_right(P0, P, Steps) :- turn(black), steps_south_west(P0, P, Steps).

%steps_ahead_left(+P0, ?P, ?Steps)
steps_ahead_left(P0, P, Steps) :- turn(white), steps_north_west(P0, P, Steps), !.  % green cut
steps_ahead_left(P0, P, Steps) :- turn(black), steps_south_east(P0, P, Steps).

%steps_behind_right(+P0, ?P, ?Steps)
steps_behind_right(P0, P, Steps) :- turn(white), steps_south_east(P0, P, Steps), !.  % green cut
steps_behind_right(P0, P, Steps) :- turn(black), steps_north_west(P0, P, Steps).

%steps_behind_left(+P0, ?P, ?Steps)
steps_behind_left(P0, P, Steps) :- turn(white), steps_south_west(P0, P, Steps), !.  % green cut
steps_behind_left(P0, P, Steps) :- turn(black), steps_north_east(P0, P, Steps).


%empty_cells(+[Points]).
empty_cells([]).
empty_cells([Point | Points]) :- 
  cell(Point, e),
  empty_cells(Points).



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
cell(point(1,2), wp). cell(point(2,2), e). cell(point(3,2), wp). cell(point(4,2), e). cell(point(5,2), wp). cell(point(6,2), wp). cell(point(7,2), wp). cell(point(8,2), wp).
cell(point(1,1), wr). cell(point(2,1), wn). cell(point(3,1), wb). cell(point(4,1), wq). cell(point(5,1), wk). cell(point(6,1), wb). cell(point(7,1), wn). cell(point(8,1), wr).

% memberchk(+Term, ?List) -> used just to check if an element is in a list, famous alternative to member(?Term, ?List). 
memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]):- memberchk(X,T).
