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


team(Piece, Color) :- member(Piece, [wp,wr,wn,wb,wq,wk]), Color = white.
team(Piece, Color) :- member(Piece, [bp,br,bn,bb,bq,bk]), Color = black.


%allies(+Piece, +Other_piece)
allies(Piece, Other_piece) :- 
  team(Piece, Color),
  team(Other_piece, Color).

%enemies(+Piece, +Other_piece)
enemies(Piece, Other_piece) :- 
  team(Piece, Color),
  team(Other_piece, Other_color),
  not(Color = Other_color).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          General chess rules                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% There are two teams (black and white) that alternate turns.
%next_turn(?Previous, ?Next)
next_turn(white, black).
next_turn(black, white).

%change_turn/0
change_turn :-
  next_turn(Current, Next),
  retract(turn(Current)),
  assert(turn(Next)),
  !.  % red cut!

%under_enemy_attack(+P)
under_enemy_attack(P) :- 
  cell(Pi, _),
  legal_move(Pi, P),
  print(Pi),
  !.  % green cut


%under_check(+Color)
under_check(Color) :-
  king(Piece),
  team(Piece, Color),
  cell(P, Piece),
  under_enemy_attack(P),
  !.  % green cut

%under_checkmate(+Color) :-
%not_under_checkmate(Color) :-
%  under_check(Color),
%  next_turn(Color, Enemy_color),
%  cell(P, P_content), (P_content = e; team(P_content, Enemy_color)),
%  ally(Allied_piece, Color),
%  cell(P0, Allied_piece),
%  move(P0, P),
%  (
%    not(under_check(Color)), undo_previous_move, !);
%    under_check(Color), undo_previous_move, 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      Legal moves (for every piece)                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%% PAWN %%%%%%%%

%legal_move(+P0, +P))
legal_move(P0, P) :-
  cell(P0, P0_content),
  pawn(P0_content),
  print("hello"),
  team(P0_content, Color),
  (%% A pawn can move one step forward to an empty cell.
    steps_forward(P0, P, 1, Color), cell(P, e)
  );
  (%% A pawn can move two cells ahead if the pawn hasn't moved yet and those two cells are both empty.
    steps_forward(P0, P, 2, Color), cell(P, e),
    steps_forward(P0, P1, 1, Color), cell(P1, e),
    history(HISTORY),
    first_move(P0, HISTORY)
  );
  (%% A pawn can move one cell diagonally ahead if there's an enemy piece in the cell of arrival.
    (steps_forward_right(P0, P, 1, Color), cell(P, P_content) ; steps_forward_left(P0, P, 1, Color), cell(P, P_content)),
    enemies(P0_content, P_content)
  ).


%%%%%%%% KNIGHT %%%%%%%%

%% A knight can move with an "L" pattern to an empty cell or to a cell containing an enemy piece.
%legal_move(+P0, +P)
legal_move(P0, P) :-
  cell(P0, P0_content),
  knight(P0_content),
  print("hello"),
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
  print("hello"),   
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
  print("hello"),
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
  print("hello"),
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
  print("hello"),
  adjacent(P0, P),
  cell(P, P_content),
  (P_content = e; enemies(P0_content, P_content)).


%% Arrocco corto
%legal_short_castle :-
%  king_never_moved,
  

%% Arrocco lungo
%legal_long_castle()




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                   Moves                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%% PUBLIC INTERFACE %%%%%%%%

%move(+Piece, +P0, +P)
move(Piece, P0, P) :-
  turn(Color),
  team(Piece, Color),
  cell(P0, Piece),
  legal_move(P0, P),
%  not(must_promote(Piece, P)),  % when reaching the last row, a pawn cannot move without promotion!
  cell(P, P_content),
  update_board(
    [cell(P0, Piece), cell(P, P_content)],  % things to retract
    [cell(P0, e), cell(P, Piece)]           % things to assert  
  ),
  change_turn.  %!.  % green cut if everything is ok... maybe useless.

%move_pawn_and_promote
%move_and_promote(Piece, Promoted_piece, P0, P) :-
%  turn(Color),
%  team(Piece, Color),
%  cell(P0, Piece),
%  legal_move(P0, P),
%  must_promote(Piece, P),
%  legal_promotion(Piece, To_piece),
%  cell(P, P_content),
%  update_board(
%    [cell(P0, Piece), cell(P, P_content)],  % things to retract
%    [cell(P0, e), cell(P, Promoted_piece)]  % things to assert  
%  ),
%  change_turn,
%  !.  % green cut if everything is ok...


%%%%%%% PRIVATE FUNCTINOALITIES %%%%%%%%

history([]).

update_board(Retracts_list, Asserts_list) :-
  retract_these(Retracts_list),
  assert_these(Asserts_list),
  retract(history(Old_history)),
  append([(Retracts_list, Asserts_list)], Old_history, Updated_history),
  assert(history(Updated_history)).

undo_last_move :-
  retract(history([(Last_retracted_list, Last_asserted_list) | Older_history])),
  retract_these(Last_asserted_list),
  assert_these(Last_retracted_list),
  assert(history(Older_history)).


first_move(P, []).
first_move(P, [(Last_retracted_list, Last_asserted_list) | Older_history]) :- 
  not(member(cell(P, _), Last_retracted_list)),
  not(member(cell(P, _), Last_asserted_list)),
  first_move(P, Older_history).


retract_these([]).
retract_these([Head | Tail]) :- retract(Head), retract_these(Tail).

assert_these([]).
assert_these([Head | Tail]) :- assert(Head), assert_these(Tail).



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


%last_row(?Row)
last_row(Row) :- player(white), Row = 8.  %% The 8th row is the last one for the white player.
last_row(Row) :- player(black), Row = 1.  %% The 1st row is the last one for the black player.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                Handy spatial predicates for a chess board                  %%
%%         (white pieces face north, while black piece face south)            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NB: Remember that, by design, "Steps" > 0.
%%     At least one between "P" and "Steps" must be istantiated.


%steps_forward(
steps_forward(P0, P, Steps, Color) :- Color = white, steps_north(P0, P, Steps).
steps_forward(P0, P, Steps, Color) :- Color = black, steps_south(P0, P, Steps).

%steps_backward
steps_backward(P0, P, Steps, Color) :- Color = white, steps_south(P0, P, Steps).
steps_backward(P0, P, Steps, Color) :- Color = black, steps_north(P0, P, Steps).

%steps_right(
steps_right(P0, P, Steps, Color) :- Color = white, steps_east(P0, P, Steps).
steps_right(P0, P, Steps, Color) :- Color = black, steps_west(P0, P, Steps).

%steps_left(
steps_left(P0, P, Steps, Color) :- Color = white, steps_west(P0, P, Steps).
steps_left(P0, P, Steps, Color) :- Color = black, steps_east(P0, P, Steps).


%steps_forward_right(
steps_forward_right(P0, P, Steps, Color) :- Color = white, steps_north_east(P0, P, Steps).
steps_forward_right(P0, P, Steps, Color) :- Color = black, steps_south_west(P0, P, Steps).

%steps_forward_left(
steps_forward_left(P0, P, Steps, Color) :- Color = white, steps_north_west(P0, P, Steps).
steps_forward_left(P0, P, Steps, Color) :- Color = black, steps_south_east(P0, P, Steps).

%steps_backward_right(
steps_backward_right(P0, P, Steps, Color) :- Color = white, steps_south_east(P0, P, Steps).
steps_backward_right(P0, P, Steps, Color) :- Color = black, steps_north_west(P0, P, Steps).

%steps_backward_left(
steps_backward_left(P0, P, Steps, Color) :- Color = white, steps_south_west(P0, P, Steps).
steps_backward_left(P0, P, Steps, Color) :- Color = black, steps_north_east(P0, P, Steps).


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
cell(point(1,7), e). cell(point(2,7), bp). cell(point(3,7), bp). cell(point(4,7), bp). cell(point(5,7), bp). cell(point(6,7), bp). cell(point(7,7), bp). cell(point(8,7), bp).
cell(point(1,6), e) . cell(point(2,6), e) . cell(point(3,6), e) . cell(point(4,6), e) . cell(point(5,6), e) . cell(point(6,6), e) . cell(point(7,6), e) . cell(point(8,6), e) .
cell(point(1,5), e) . cell(point(2,5), e) . cell(point(3,5), e) . cell(point(4,5), e) . cell(point(5,5), e) . cell(point(6,5), e) . cell(point(7,5), e) . cell(point(8,5), e) .
cell(point(1,4), e) . cell(point(2,4), e) . cell(point(3,4), e) . cell(point(4,4), e) . cell(point(5,4), e) . cell(point(6,4), e) . cell(point(7,4), e) . cell(point(8,4), e) .
cell(point(1,3), e) . cell(point(2,3), e) . cell(point(3,3), e) . cell(point(4,3), e) . cell(point(5,3), e) . cell(point(6,3), e) . cell(point(7,3), e) . cell(point(8,3), e) .
cell(point(1,2), wp). cell(point(2,2), e). cell(point(3,2), wp). cell(point(4,2), kkk). cell(point(5,2), wp). cell(point(6,2), wp). cell(point(7,2), wp). cell(point(8,2), wp).
cell(point(1,1), wr). cell(point(2,1), wn). cell(point(3,1), wb). cell(point(4,1), wq). cell(point(5,1), wk). cell(point(6,1), wb). cell(point(7,1), wn). cell(point(8,1), wr).



% memberchk(+Term, ?List) -> used just to check if an element is in a list, famous alternative to member(?Term, ?List). 
memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]):- memberchk(X,T).
