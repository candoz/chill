:- include(space.pl).
:- include('chess_rules.pl').

%% Chess board at the beginning of a new game:
% Black Rook  . Black kNight  . Black Bishop  . Black Queen   . Black King    . Black Bishop  . Black kNight  . Black Rook    .
% Black Pawn  . Black Pawn    . Black Pawn    . Black Pawn    . Black Pawn    . Black Pawn    . Black Pawn    . Black Pawn    .
cell(1, 8, br). cell(2, 8, bn). cell(3, 8, bb). cell(4, 8, bq). cell(5, 8, bk). cell(6, 8, bb). cell(7, 8, bn). cell(8, 8, br). % 8
cell(1, 7, bp). cell(2, 7, bp). cell(3, 7, bp). cell(4, 7, bp). cell(5, 7, bp). cell(6, 7, bp). cell(7, 7, bp). cell(8, 7, bp). % 7
cell(1, 6, e) . cell(2, 6, e) . cell(3, 6, e) . cell(4, 6, e) . cell(5, 6, e) . cell(6, 6, e) . cell(7, 6, e) . cell(8, 6, e) . % 6  (Empty row)
cell(1, 5, e) . cell(2, 5, e) . cell(3, 5, e) . cell(4, 5, e) . cell(5, 5, e) . cell(6, 5, e) . cell(7, 5, e) . cell(8, 5, e) . % 5  (Empty row)
cell(1, 4, e) . cell(2, 4, e) . cell(3, 4, e) . cell(4, 4, e) . cell(5, 4, e) . cell(6, 4, e) . cell(7, 4, e) . cell(8, 4, e) . % 4  (Empty row)
cell(1, 3, e) . cell(2, 3, e) . cell(3, 3, e) . cell(4, 3, e) . cell(5, 3, e) . cell(6, 3, e) . cell(7, 3, e) . cell(8, 3, e) . % 3  (Empty row)
cell(1, 2, wp). cell(2, 2, wp). cell(3, 2, wp). cell(4, 2, wp). cell(5, 2, wp). cell(6, 2, wp). cell(7, 2, wp). cell(8, 2, wp). % 2
cell(1, 1, wr). cell(2, 1, wn). cell(3, 1, wb). cell(4, 1, wq). cell(5, 1, wk). cell(6, 1, wb). cell(7, 1, wn). cell(8, 1, wr). % 1
% White Pawn  . White Pawn    . White Pawn    . White Pawn    . White Pawn    . White Pawn    . White Pawn    . White Pawn    .
% White Rook  . White kNight  . White Bishop  . White Queen   . White King    . White Bishop  . White kNight  . White Rook    .
%     a       .       b       .       c       .       d       .       e       .       f       .       g       .       h       .

%turn(?COLOR)
turn(white).

%change_turn/0
change_turn :-
  next_turn(THIS, NEXT),
  retract(turn(THIS)),
  assert(turn(NEXT)),
  !.  % red cut!
%% old, alternative implementation without next_turn/2 %% 
%change_turn :- retract(turn(white)), assert(turn(black)), !.  % red cut
%change_turn :- retract(turn(black)), assert(turn(white)), !.  % green cut



%print_board_for_white() :-
%print_board_for_black() :-
%print_board() :-
%  (bottom(white), print_board_for_white());
%  (bottom(black), print_board_for_balck()).


%%% Things to keep track of %%%

%last_move(_, _, _, _, _)
%wk_moved(false)
%bk_moved(false)


%move(+PIECE, +BEFORE_X, +BEFORE_Y, +AFTER_X, +AFTER_Y)
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

lets_move(PIECE, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  retract(cell(BEFORE_X, BEFORE_Y, PIECE)), assert(cell(BEFORE_X, BEFORE_Y, e)),
  retract(cell(AFTER_X, AFTER_Y, _)), assert(cell(AFTER_X, AFTER_Y, PIECE)).

  
  