:- include('space_2d.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Pieces and teams                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pawn(wp). pawn(bp).      % white and black Pawns
rook(wr). rook(br).      % white and black Rooks
knight(wn). knight(bn).  % white and black kNights
bishop(wb). bishop(bb).  % white and black Bishops
queen(wq). queen(bq).    % white and black Queens
king(wk). king(bk).      % white and balck Kings

%team(?Piece, ?Color)
team(Piece, Color) :- Color = white, member(Piece, [wp,wr,wn,wb,wq,wk]).
team(Piece, Color) :- Color = black, member(Piece, [bp,br,bn,bb,bq,bk]).


%enemy(+Piece)
enemy(Piece) :- turn(white), black(Piece), !.  % green cut
enemy(Piece) :- turn(black), white(Piece).

%ally(+Piece)
allied(Piece) :- turn(white), white(Piece), !.  % green cut
allied(Piece) :- turn(black), black(Piece).


%enemies(+Piece, +Ohter_piece)
enemies(Piece_1, Ohter_piece) :- 
  team(Piece, Color),
  team(Other_piece), !.  % green cut
enemies(Piece, Ohter_piece) :- black(Piece), white(Other_piece).

%allies(+Piece, +Ohter_piece)
allied(Piece, Ohter_piece) :- white(Piece), white(Other_piece), !.  % green cut
allied(Piece, Ohter_piece) :- black(Piece), black(Other_piece).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          General chess rules                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% There are two teams (black and white) that alternate turns.
%next_turn(?CURRENT, ?NEXT)
next_turn(white, black).
next_turn(black, white).


%under_enemy_attack(+P)
under_enemy_attack(P) :- 
  cell(P, Piece),
  cell(Pi, Other_Piece),
  enemies(Piece, Other_Piece),
  legal_move(Pi, P).


under_check(Team) :-
  king(Piece),
  ally(Piece),
  cell(P, Piece),
  under_enemy_attack(P).


%under_checkmate() :-



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      Legal moves (for every piece)                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%% PAWN %%%%%%%%

%legal_move(+P0, +P))
legal_move(P0, P) :-
  cell(P0, P0_content),
  pawn(P0_content),
  (%% A pawn can move one step forward to an empty cell.
    steps_ahead(P0, P, 1), cell(P, e)
  );
  (%% A pawn can move two cells ahead if the pawn is at its starting row and those two cells are both empty.
    starting_row(Piece, Y0),
    steps_ahead(P0, P1, 1), cell(P1, e),
    steps_ahead(P0, P, 2), cell(P, e)
  );
  (%% A pawn can move one cell diagonally ahead if there's an enemy piece in the cell of arrival.
    steps_ahead_right(P0, P, 1), cell(P, Content),
    enemy(Content)
  ).


%%%%%%%% KNIGHT %%%%%%%%

%% A knight can move with an "L" pattern to an empty cell or to a cell containing an enemy piece.
%legal_move(+P0, +P)
legal_move(P0, P) :-
  cell(P0, P0_content),
  knight(P0_content),
  l_pattern(P0, P),
  cell(P, P_content),
  (P_content = e; enemies(P0_content, P_content)).


%%%%%%%% BISHOP %%%%%%%%

%% A bishop can move diagonlly to an empty cell or to a cell containing an enemy piece.
%% All the cells between the starting point (P0) and the ending point (P) must be empty.
%legal_move(+P0, +P)
legal_move(P0, P) :-
  cell(P0, P0_content),
  bishop(P0_content),     
  aligned_diagonally(P0, P),
  ((in_between(P0, P, Points), empty_cells(Points)); not(in_between(P0, P, Points))),
  cell(P, P_content),
  (P_content = e; enemies(P0_content, P_content)).


%%%%%%%% ROOK %%%%%%%%

%% A rook can move horizontally or vertically to an empty cell or to a cell containing an enemy piece.
%% All the cells between the starting point (P0) and the ending point (P) must be empty.
%legal_move(+P0, +P)
legal_move(P0, P) :-
  cell(P0, P0_content),
  rook(P0_content), 
  aligned_axis(P0, P),
  ((in_between(P0, P, Points), empty_cells(Points)); not(in_between(P0, P, Points))),
  cell(P, P_content),
  (P_content = e; enemies(P0_content, P_content)).


%%%%%%%% QUEEN %%%%%%%%

%% The queen can move horizontally, vertically or diagonally to an empty cell or to a cell containing an enemy piece. 
%% All the cells between the starting point (P0) and the ending point (P) must be empty.
%legal_move(+P0, +P)
legal_move(P0, P) :-
  cell(P0, P0_content),
  queen(P0_content), 
  aligned(P0, P),
  ((in_between(P0, P, Points), empty_cells(Points)); not(in_between(P0, P, Points))),
  cell(P, P_content),
  (P_content = e; enemies(P0_content, P_content)).


%%%%%%%% KING %%%%%%%%

%% The king can move to all the cells adjacent around him, but only if they are empty or contain an enemy.
%legal_move(+P0, +P)
legal_move(P0, P) :-
  cell(P0, P0_content),
  king(P0_content), 
  adjacent(P0, P),
  cell(P, P_content),
  (P_content = e; enemies(P0_content, P_content)).


%% Arrocco corto
%legal_short_castle :-
%  king_never_moved,
  

%% Arrocco lungo
%legal_long_castle()



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      Relevant chess facts and rules                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%must_promote(+Piece, +P)
must_promote(Piece, P) :-  % Pawns must promote when reaching the last row.
  pawn(Piece),
  last_row(P).

%legal_promotion(+Piece, +To_piece)
legal_promotion(Piece, To_piece) :-  % Promotion can be to a queen, a knight, a rook or a bishop of the same team.
  ally(To_piece),
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
last_row(Row) :- player(white), Row = 8.  %% The 8th row is the last one for the white player.
last_row(Row) :- player(black), Row = 1.  %% The 1st row is the last one for the black player.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                Handy spatial predicates for a chess board                  %%
%%         (white pieces face north, while black piece face south)            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NB: Remember that, by design, "Steps" > 0.
%%     At least one between "P" and "Steps" must be istantiated.


%steps_ahead(+Piece, +P0, ?P, ?Steps)
steps_ahead(Piece, P0, P, Steps) :- white(Piece), steps_north(P0, P, Steps), !.  % green cut
steps_ahead(Piece, P0, P, Steps) :- black(Piece), steps_south(P0, P, Steps).

%steps_behind(+P0, ?P, ?Steps)
steps_behind(Piece, P0, P, Steps) :- white(Piece), steps_south(P0, P, Steps), !.  % green cut
steps_behind(Piece, P0, P, Steps) :- black(Piece), steps_north(P0, P, Steps).

%steps_right(+P0, ?P, ?Steps)
steps_right(P0, P, Steps) :- player(white), steps_east(P0, P, Steps), !.  % green cut
steps_right(P0, P, Steps) :- player(black), steps_west(P0, P, Steps).

%steps_left(+P0, ?P, ?Steps)
steps_left(P0, P, Steps) :- player(white), steps_west(P0, P, Steps), !.  % green cut
steps_left(P0, P, Steps) :- player(black), steps_east(P0, P, Steps).


%steps_ahead_right(+P0, ?P, ?Steps)
steps_ahead_right(P0, P, Steps) :- player(white), steps_north_east(P0, P, Steps), !.  % green cut
steps_ahead_right(P0, P, Steps) :- player(black), steps_south_west(P0, P, Steps).

%steps_ahead_left(+P0, ?P, ?Steps)
steps_ahead_left(P0, P, Steps) :- player(white), steps_north_west(P0, P, Steps), !.  % green cut
steps_ahead_left(P0, P, Steps) :- player(black), steps_south_east(P0, P, Steps).

%steps_behind_right(+P0, ?P, ?Steps)
steps_behind_right(P0, P, Steps) :- player(white), steps_south_east(P0, P, Steps), !.  % green cut
steps_behind_right(P0, P, Steps) :- player(black), steps_north_west(P0, P, Steps).

%steps_behind_left(+P0, ?P, ?Steps)
steps_behind_left(P0, P, Steps) :- player(white), steps_south_west(P0, P, Steps), !.  % green cut
steps_behind_left(P0, P, Steps) :- player(black), steps_north_east(P0, P, Steps).


%empty_cells(+[Points]).
empty_cells([]).
empty_cells([Point | Points]) :- 
  cell(Point, e),
  empty_cells(Points).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             D E B U G G I N G   (and old stuff)            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

turn(white).
player(white).

cell(point(1,8), br). cell(point(2,8), bn). cell(point(3,8), bb). cell(point(4,8), bq). cell(point(5,8), bk). cell(point(6,8), bb). cell(point(7,8), bn). cell(point(8,8), br).
cell(point(1,7), e). cell(point(2,7), bp). cell(point(3,7), bp). cell(point(4,7), bp). cell(point(5,7), bp). cell(point(6,7), bp). cell(point(7,7), bp). cell(point(8,7), bp).
cell(point(1,6), e) . cell(point(2,6), e) . cell(point(3,6), e) . cell(point(4,6), e) . cell(point(5,6), e) . cell(point(6,6), e) . cell(point(7,6), e) . cell(point(8,6), e) .
cell(point(1,5), e) . cell(point(2,5), e) . cell(point(3,5), e) . cell(point(4,5), e) . cell(point(5,5), e) . cell(point(6,5), e) . cell(point(7,5), e) . cell(point(8,5), e) .
cell(point(1,4), e) . cell(point(2,4), e) . cell(point(3,4), e) . cell(point(4,4), e) . cell(point(5,4), e) . cell(point(6,4), e) . cell(point(7,4), e) . cell(point(8,4), e) .
cell(point(1,3), e) . cell(point(2,3), e) . cell(point(3,3), e) . cell(point(4,3), e) . cell(point(5,3), e) . cell(point(6,3), e) . cell(point(7,3), e) . cell(point(8,3), e) .
cell(point(1,2), wp). cell(point(2,2), e). cell(point(3,2), wp). cell(point(4,2), e). cell(point(5,2), wp). cell(point(6,2), wp). cell(point(7,2), wp). cell(point(8,2), wp).
cell(point(1,1), wr). cell(point(2,1), wn). cell(point(3,1), wb). cell(point(4,1), wq). cell(point(5,1), wk). cell(point(6,1), wb). cell(point(7,1), wn). cell(point(8,1), wr).



% memberchk(+Term, ?List) -> used just to check if an element is in a list, famous alternative to member(?Term, ?List). 
memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]):- memberchk(X,T).
