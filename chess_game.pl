:- include('space_2d.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                Game state                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%    Black Rook    |    Black kNight    |    Black Bishop    |     Black Queen    |     Black King     |    Black Bishop    |    Black kNight    |     Black Rook     |
%    Black Pawn    |     Black Pawn     |     Black Pawn     |     Black Pawn     |     Black Pawn     |     Black Pawn     |     Black Pawn     |     Black Pawn     |
cell(point(0,7),br). cell(point(1,7),bn). cell(point(2,7),bb). cell(point(3,7),bq). cell(point(4,7),bk). cell(point(5,7),bb). cell(point(6,7),bn). cell(point(7,7),br).
cell(point(0,6),bp). cell(point(1,6),bp). cell(point(2,6),bp). cell(point(3,6),bp). cell(point(4,6),bp). cell(point(5,6),bp). cell(point(6,6),bp). cell(point(7,6),bp).
cell(point(0,5),e).  cell(point(1,5),e).  cell(point(2,5),e).  cell(point(3,5),e).  cell(point(4,5),e).  cell(point(5,5),e).  cell(point(6,5),e).  cell(point(7,5),e). % empty row
cell(point(0,4),e).  cell(point(1,4),e).  cell(point(2,4),e).  cell(point(3,4),e).  cell(point(4,4),e).  cell(point(5,4),e).  cell(point(6,4),e).  cell(point(7,4),e). % empty row
cell(point(0,3),e).  cell(point(1,3),e).  cell(point(2,3),e).  cell(point(3,3),e).  cell(point(4,3),e).  cell(point(5,3),e).  cell(point(6,3),e).  cell(point(7,3),e). % empty row
cell(point(0,2),e).  cell(point(1,2),e).  cell(point(2,2),e).  cell(point(3,2),e).  cell(point(4,2),e).  cell(point(5,2),e).  cell(point(6,2),e).  cell(point(7,2),e). % empty row
cell(point(0,1),wp). cell(point(1,1),wp). cell(point(2,1),wp). cell(point(3,1),wp). cell(point(4,1),wp). cell(point(5,1),wp). cell(point(6,1),wp). cell(point(7,1),wp).
cell(point(0,0),wr). cell(point(1,0),wn). cell(point(2,0),wb). cell(point(3,0),wq). cell(point(4,0),wk). cell(point(5,0),wb). cell(point(6,0),wn). cell(point(7,0),wr).
%    White Pawn    |     White Pawn     |     White Pawn     |     White Pawn     |     White Pawn     |     White Pawn     |     White Pawn     |     White Pawn     .
%    White Rook    |    White kNight    |    White Bishop    |     White Queen    |     White King     |    White Bishop    |    White kNight    |     White Rook     .

%%% Indicates the current turn: Color can be “black” or “white”
% turn(-Color)
turn(white).


%%% Will contain a list of couples.
%%% Each couple is formed by a list of retracted cells and a list of asserted cells;
%%% this way, it's possible to keep track of every update done on the board
% history(-Board_updates_list)
history([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%              Pieces, teams and current result information                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pawn(wp).   pawn(bp).    % white and black Pawns
rook(wr).   rook(br).    % white and black Rooks
king(wk).   king(bk).    % white and balck Kings
queen(wq).  queen(bq).   % white and black Queens
knight(wn). knight(bn).  % white and black kNights
bishop(wb). bishop(bb).  % white and black Bishops

% team(?Piece, ?Color)
team(Piece, white) :- member(Piece, [wp,wr,wn,wb,wq,wk]).
team(Piece, black) :- member(Piece, [bp,br,bn,bb,bq,bk]).

% allies(?Piece1, ?Piece2)
allies(Piece1, Piece2) :-
  team(Piece1, Color),
  team(Piece2, Color).

% enemies(?Piece1, ?Piece2)
enemies(Piece1, Piece2) :-
  team(Piece1, Color1),
  team(Piece2, Color2),
  Color1 \= Color2.


%%% Evaluates the current game status: R can be “white_won” or “black_won” (by checkmate),
%%% “draw” (by stalemate), “white_in_check”, “black_in_check”, or “nothing”.
% result(-R)
result(white_won) :- checkmate, turn(black), !.
result(black_won) :- checkmate, turn(white), !.
result(draw) :- stalemate, !.
% result(draw) :- only_kings_on_board, !.
result(white_in_check) :- turn(white), under_check(white), !.
result(black_in_check) :- turn(black), under_check(black), !.
result(nothing).


%%% Retrieves all the chessboard cells in a list, following the usual format
%%% "cell(point(X,Y),Piece)" for every item of the list
% chessboard(-Cells_list)
chessboard(Cells_list) :- findall(cell(P,Content), cell(P,Content), Cells_list).

%%% Retrieves all the chessboard cells in a list, following the compact format
%%% "[X,Y,Piece]" for every item of the list
% chessboard_compact(-Compact_list)
chessboard_compact(Compact_list) :- findall([X,Y,Content], cell(point(X,Y), Content), Compact_list).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           General functionalities for history and staus update             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% next_turn(?Previous, ?Next)
next_turn(white, black).
next_turn(black, white).

% change_turn
change_turn :-
  turn(Current),
  next_turn(Current, Next),
  retract(turn(Current)),
  assert(turn(Next)),
  !. % red cut!


%%% Updates the game status:
%%% - retracts all the cells inside the To_retract list
%%% - asserts all the cells inside the To_assert list
%%% - updates the history, adding this update
% update_board(+To_retract, +To_assert)
update_board(To_retract, To_assert) :-
  retract_these(To_retract),
  assert_these(To_assert),
  retract(history(Old_history)),
  append([(To_retract, To_assert)], Old_history, Updated_history),
  assert(history(Updated_history)).

%%% Reverts the last update thanks to the info inside history:
%%% - retracts all the cells that had been asserted in the last board update
%%% - asserts all the cells that had been retracted in the last board update
%%% - updates the history, erasing the last update
% undo_last_update
undo_last_update :-
  retract(history([(Last_retracted_list, Last_asserted_list) | Older_history])),
  retract_these(Last_asserted_list),
  assert_these(Last_retracted_list),
  assert(history(Older_history)).


%%% Evaluates if the Color team will be under check after having simulated the retraction
%%% of the Cells_to_retract and the following assertion of the Cells_to_assert.
% new_position_not_under_check(+Cells_to_retract, +Cells_to_assert, +Color)
new_position_not_under_check(Cells_to_retract, Cells_to_assert, Color) :-
  update_board(Cells_to_retract, Cells_to_assert),
  (
    (
      not under_check(Color),
      undo_last_update,  % modifying the knowledge base! (and changing turn...)
      true,
      !
    ) ; (
      under_check(Color),
      undo_last_update,  % modifying the knowledge base! (and changing turn...)
      fail
    )
  ).

%%% Retrieves a list containing the cells involved in the last movement,
%%% following the usual format "cell(point(X,Y),piece)" for every list item
% last_moved(-Points_list)
last_moved([]) :- hitory([]), !. % green cut
last_moved(Last_asserted_list) :- history([(_, Last_asserted_list) | _]).

%%% Retrieves a list containing the cells involved in the last movement,
%%% following the compact format "[X,Y,piece]" for every list item
% last_moved_compact(-Coordinates_list)
last_moved_compact([]) :- last_moved([]).
last_moved_compact([[X1,Y1], [X2,Y2]]) :- last_moved([cell(point(X1,Y1),_), cell(point(X2,Y2),_)]).
last_moved_compact([[X1,Y1], [X2,Y2], [X3,Y3], [X4,Y4]]) :- % for castling, where 4 cells are involved
  last_moved([cell(point(X1,Y1),_), cell(point(X2,Y2),_), cell(point(X3,Y3),_), cell(point(X4,Y4),_)]).


%%% Utility for multiple retractions
% retract_these(+List)
retract_these([]).
retract_these([Head | Tail]) :- retract(Head), retract_these(Tail).

%%% Utility for multiple assertions
% assert_these(+List)
assert_these([]).
assert_these([Head | Tail]) :- assert(Head), assert_these(Tail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%               Some chess concepts and other useful stuff                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% under_attack(+P)
under_attack(P) :-
  cell(Pi, Piece),
  legal_move(Piece, Pi, P), % ; legal_castle(Piece, Pi, P) ),
  !.  % green cut

% under_check(+Color)
under_check(Color) :-
  king(K),
  team(K, Color),
  cell(P, K),
  under_attack(P),
  !.  % green cut


% stalemate
stalemate :-
  turn(Color),
  not under_check(Color),
  not can_move(Color).

% checkmate
checkmate :-
  turn(Color),
  under_check(Color),
  not can_move(Color).


%%% Pawns must promote when reaching the last row.
% must_promote(+Piece, +P)
must_promote(Piece, P) :-
  pawn(Piece),
  team(Piece, Color),
  last_row(P, Color).

% Pawns can promote to a queen, a knight, a bishop or a rook of the same team.
% legal_promotion(+Piece, ?To_piece)
legal_promotion(Piece, To_piece) :-
  allies(Piece, To_piece),
  ( queen(To_piece) ; knight(To_piece) ; bishop(To_piece) ; rook(To_piece) ).


% only_kings_on_board
only_kings_on_board :-
  not cell(_,wp), not cell(_,bp), % no pawns
  not cell(_,wr), not cell(_,br), % no rooks
  not cell(_,wn), not cell(_,bn), % no knights
  not cell(_,wb), not cell(_,bb), % no bishops
  not cell(_,wq), not cell(_,bq). % no queens


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                Specific functionalities for movements                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% True for every point P of the board still not involved in any movement:
%%% no piece has moved to or from that point since the beginning of the game.
% never_moved(?P)
never_moved(P) :-
  history(H),
  cell(P, P_content),
  never_moved(cell(P, P_content), H).

% never_moved(+Cell, +History)
never_moved(Cell, []).
never_moved(Cell, [(Last_retracted_list, Last_asserted_list) | Older_history]) :-
  % not member(Cell, Last_retracted_list),
  not member(Cell, Last_asserted_list),
  never_moved(Cell, Older_history).


% can_move(+Color)
can_move(Color) :-
  team(Ally, Color),
  cell(P0, Ally),
  legal_move(Ally, P0, _),
  !. % green cut


% available_move(+P0, ?P)
available_move(P0, P) :- cell(P0, Piece), legal_move(Piece, P0, P).
available_move(P0, P) :- cell(P0, Piece), legal_castle(Piece, P0, P).

%%% Retrieves a list containing all the cells to which the (eventual) piece residing in P0 can move to.
%%% The list items are presented in the format "(point(X,Y)"
% available_moves(+P0, -Points_list)
available_moves(P0, Points_list) :- findall(P, available_move(P0, P), Points_list).

%%% Retrieves a list containing all the cells to which the (eventual) piece residing in point(X0,Y0) can move to.
%%% The list items are presented in the compact format "[X,Y]"
% available_moves([+X0,+Y0], -Compact_list)
available_moves_compact([X0,Y0], Compact_list) :- findall([X,Y], available_move(point(X0,Y0), point(X,Y)), Compact_list).


%%% Move the Piece, if possible, from the point P0 to the point P.
%%% As a consequence, the board will be updated and the turn will be changed.
%%% Use this for every move except for promotions, short castling and long castling.
% do_move(+Piece, +P0, +P)
do_move(Piece, P0, P) :-
  not must_promote(Piece, P), % when reaching the last row, a pawn cannot move without promotion
  turn(Color),
  team(Piece, Color),
  legal_move(Piece, P0, P),
  cell(P, P_content),
  update_board(
    [cell(P0,Piece), cell(P,P_content)], % things to retract
    [cell(P0,e), cell(P,Piece)]          % things to assert
  ),
  change_turn.

%%% Move the Piece, if possible, from P0 to P and then promote to the Promoted_piece.
%%% As a consequence, the board will be updated and the turn will be changed.
%%% Use this predicate only when a pawn is reaching the final row and, therefore, must promote.
% do_move_and_promote(+Piece, +P0, +P, +Promoted_piece)
do_move_and_promote(Piece, P0, P, Promoted_piece) :-
  must_promote(Piece, P),
  legal_promotion(Piece, Promoted_piece),
  turn(Color),
  team(Piece, Color),
  legal_move(Piece, P0, P),
  cell(P, P_content),
  update_board(
    [cell(P0, Piece), cell(P, P_content)],  % things to retract
    [cell(P0, e), cell(P, Promoted_piece)]  % things to assert
  ),
  change_turn.


%%% Execute, if possible, a short castle for the King in P0.
%%% As a consequence, the position of both the king and the corresponding rook will be updated on the board,
%%% and the turn will be changed.
% do_short_castle(+King, +P0)
do_short_castle(King, P0) :-
  turn(Color),
  team(King, Color),
  steps_east(P0, P2, 2), % where the king wants to go
  legal_castle(King, P0, P2),
  steps_east(P0, P1, 1),
  steps_east(P0, P3, 3), % where the allied tower is
  update_board_for_castle(King, P0, P1, P2, P3),
  change_turn.

%%% Execute, if possible, a long castle for the King in P0.
%%% As a consequence, the position of both the king and the corresponding rook will be updated on the board,
%%% and the turn will be changed.
% do_long_castle(+King, +P0)
do_long_castle(King, P0) :-
  turn(Color),
  team(King, Color),
  steps_west(P0, P2, 2), % where the king wants to go
  legal_castle(King, P0, P2),
  steps_west(P0, P1, 1),
  steps_west(P0, P4, 4), % where the allied tower is
  update_board_for_castle(King, P0, P1, P2, P4),
  change_turn.


% update_board_for_castle(+King, +P0, +P1, +P2, +PR)
update_board_for_castle(King, P0, P1, P2, PR) :-
  cell(PR, Rook),
  update_board(
    [cell(P0,King), cell(P1,e), cell(P2,e), cell(PR,Rook)], % things to retract
    [cell(P0,e), cell(P1,Rook), cell(P2,King), cell(PR,e)]  % things to assert
  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            Standard legal moves by the book (for every piece)              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% legal_move(?Piece, +P0, ?P)
legal_move(Piece, P0, P) :-
  cell(P0,Piece),
  cell(P,P_content),
  legal(cell(P0,Piece), cell(P,P_content)),
  team(Piece, Color),
  (
    new_position_not_under_check(
      [cell(P0,Piece), cell(P,P_content)],
      [cell(P0,e), cell(P,Piece)],
      Color
    ) ; ( % NB !!!
      king(P_content),
      turn(Current_turn_color),
      team(P_content, Current_turn_color)
    )
  ).


%%% PAWN %%%

legal(cell(P0,P0_content), cell(P,e)) :-
  pawn(P0_content),
  steps_forward(P0, P, 1).

legal(cell(P0,P0_content), cell(P,e)) :-
  pawn(P0_content),
  steps_forward(P0, P, 2),
  never_moved(P0),
  steps_forward(P0, P1, 1),
  cell(P1, e).

legal(cell(P0,P0_content), cell(P,P_content)) :-
  pawn(P0_content),
  ( steps_forward_right(P0, P, 1) ; steps_forward_left(P0, P, 1) ),
  enemies(P0_content, P_content).

%%% KNIGHT %%%

legal(cell(P0,P0_content), cell(P,P_content)) :-
  knight(P0_content),
  l_pattern(P0, P),
  empty_or_enemy(P0_content, P_content).

%%% BISHOP %%%

legal(cell(P0,P0_content), cell(P,P_content)) :-
  bishop(P0_content),
  aligned_diagonal(P0, P),
  no_pieces_interposed(P0, P),
  empty_or_enemy(P0_content, P_content).

%%% ROOK %%%

legal(cell(P0,P0_content), cell(P,P_content)) :-
  rook(P0_content),
  aligned_axis(P0, P),
  no_pieces_interposed(P0, P),
  empty_or_enemy(P0_content, P_content).

%%% QUEEN %%%

legal(cell(P0,P0_content), cell(P,P_content)) :-
  queen(P0_content),
  aligned(P0, P),
  no_pieces_interposed(P0, P),
  empty_or_enemy(P0_content, P_content).

%%% KING %%%

legal(cell(P0,P0_content), cell(P,P_content)) :- % excluding castling, see below...
  king(P0_content),
  adjacent(P0, P),
  empty_or_enemy(P0_content, P_content).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        Legal castling by the book                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% legal_castle(+Piece, P0, P)
legal_castle(Piece, P0, P) :-
  cell(P0, Piece),
  king(Piece),
  never_moved(P0),
  not under_attack(P0),
  ( legal_short(Piece, P0, P) ; legal_long(Piece, P0, P) ).


legal_short(Piece, P0, P2) :-
  steps_east(P0, P2, 2), % where the king wants to go
  steps_east(P0, P1, 1),
  steps_east(P0, P3, 3), % east rook position
  cell(P1, e),
  cell(P2, e),
  never_moved(P3),
  team(Piece, Color),
  new_position_not_under_check([cell(P0,Piece), cell(P1,e)], [cell(P0,e), cell(P1,Piece)], Color),
  new_position_not_under_check([cell(P0,Piece), cell(P2,e)], [cell(P0,e), cell(P2,Piece)], Color).

legal_long(Piece, P0, P2) :-
  steps_west(P0, P2, 2), % where the king wants to go
  steps_west(P0, P1, 1),
  steps_west(P0, P3, 3),
  steps_west(P0, P4, 4), % west rook position
  cell(P1, e),
  cell(P2, e),
  cell(P3, e),
  never_moved(P4),
  team(Piece, Color),
  new_position_not_under_check([cell(P0,Piece), cell(P1,e)], [cell(P0,e), cell(P1,Piece)], Color),
  new_position_not_under_check([cell(P0,Piece), cell(P2,e)], [cell(P0,e), cell(P2,Piece)], Color).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                Handy spatial predicates for a chess board                  %%
%%         (white pieces face north, while black piece face south)            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NB: Remember that, by design, "Steps" > 0.
%%     At least one between "P" and "Steps" must be istantiated.


% steps
steps_forward(P0, P, Steps, white) :- steps_north(P0, P, Steps).
steps_forward(P0, P, Steps, black) :- steps_south(P0, P, Steps).
steps_forward(P0, P, Steps) :- cell(P0, Piece), team(Piece, Color), steps_forward(P0, P, Steps, Color).

% steps_backward
steps_backward(P0, P, Steps, Color) :- Color = white, steps_south(P0, P, Steps).
steps_backward(P0, P, Steps, Color) :- Color = black, steps_north(P0, P, Steps).
steps_backward(P0, P, Steps) :- cell(P0, Piece), team(Piece, Color), steps_backward(P0, P, Steps, Color).

% steps_right
steps_right(P0, P, Steps, Color) :- Color = white, steps_east(P0, P, Steps).
steps_right(P0, P, Steps, Color) :- Color = black, steps_west(P0, P, Steps).
steps_right(P0, P, Steps) :- cell(P0, Piece), team(Piece, Color), steps_right(P0, P, Steps, Color).

% steps_left
steps_left(P0, P, Steps, Color) :- Color = white, steps_west(P0, P, Steps).
steps_left(P0, P, Steps, Color) :- Color = black, steps_east(P0, P, Steps).
steps_left(P0, P, Steps) :- cell(P0, Piece), team(Piece, Color), steps_left(P0, P, Steps, Color).


% steps_forward_right
steps_forward_right(P0, P, Steps, Color) :- Color = white, steps_north_east(P0, P, Steps).
steps_forward_right(P0, P, Steps, Color) :- Color = black, steps_south_west(P0, P, Steps).
steps_forward_right(P0, P, Steps) :- cell(P0, Piece), team(Piece, Color), steps_forward_right(P0, P, Steps, Color).

% steps_forward_left
steps_forward_left(P0, P, Steps, Color) :- Color = white, steps_north_west(P0, P, Steps).
steps_forward_left(P0, P, Steps, Color) :- Color = black, steps_south_east(P0, P, Steps).
steps_forward_left(P0, P, Steps) :- cell(P0, Piece), team(Piece, Color), steps_forward_left(P0, P, Steps, Color).

% steps_backward_right
steps_backward_right(P0, P, Steps, Color) :- Color = white, steps_south_east(P0, P, Steps).
steps_backward_right(P0, P, Steps, Color) :- Color = black, steps_north_west(P0, P, Steps).
steps_backward_right(P0, P, Steps) :- cell(P0, Piece), team(Piece, Color), steps_backward_right(P0, P, Steps, Color).

% steps_backward_left
steps_backward_left(P0, P, Steps, Color) :- Color = white, steps_south_west(P0, P, Steps).
steps_backward_left(P0, P, Steps, Color) :- Color = black, steps_north_east(P0, P, Steps).
steps_backward_left(P0, P, Steps) :- cell(P0, Piece), team(Piece, Color), steps_backward_left(P0, P, Steps, Color).


% last_row(+P, ?Color)
last_row(point(_,7), white).
last_row(point(_,0), black).

% empty_or_enemy(+Piece1, ?Piece2)
empty_or_enemy(Piece1, Piece2) :- enemies(Piece1, Piece2).
empty_or_enemy(_, e).

% no_pieces_interposed(+P0, +P)
no_pieces_interposed(P0, P) :- adjacent(P0, P).
no_pieces_interposed(P0, P) :- in_between(P0, P, Points), empty_cells(Points).

% empty_cells(+[Points]).
empty_cells([]).
empty_cells([Point | Points]) :-
  cell(Point, e),
  empty_cells(Points).
