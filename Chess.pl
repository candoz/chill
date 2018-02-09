turn(white).   % white or black; white always start

% Raw board initial representation: good enough for low level computations.
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

print_board_for_white() :-
print_board_for_black() :-
print_board() :-
  (bottom(white), print_board_for_white());
  (bottom(black), print_board_for_balck()).

% Aliases for canonical horizontal coordinates
cell(a, _, _) :- cell(1, _, _).
cell(b, _, _) :- cell(2, _, _).
cell(c, _, _) :- cell(3, _, _).
cell(d, _, _) :- cell(4, _, _).
cell(e, _, _) :- cell(5, _, _).
cell(f, _, _) :- cell(6, _, _).
cell(g, _, _) :- cell(7, _, _).
cell(h, _, _) :- cell(8, _, _).

% I think I need to keep track of some things...
%%last_move(_, _, _, _, _)
%%wk_moved(false)
%%bk_moved(false)

% Maybe these can be useful
is_white(PIECE) :- PIECE = (wp; wr; wn; wb; wq; wk).
is_black(PIECE) :- PIECE = (bp; br; bn; bb; bq; bk).
is_legal(PIECE) :- is_white(PIECE); is_balck(PIECE).

% Pawn promotion rules
%%promote()

% move(+PAWN, +BEFORE_X, +BEFORE_Y, +AFTER_X, +AFTER_Y)
move(PAWN, BEFORE_X, BEFORE_Y, AFTER_X, AFTER_Y) :-
  cell(BEFORE_X, BEFORE_Y, PAWN).                                      % is there a pawn of the right color in the 'before' position?
  ((PAWN = wp), cell() (turn(white))) ; ((PAWN = bp), (turn(black))),  % is it really a pawn what you're trying to move? And is it of the right color?
  
  