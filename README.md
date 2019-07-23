# Chess in Logic Language
*ChiLL* is an implementation of a chessboard in Prolog.  
It's possible to interact with it through a set of queries and carry on a legal chess game between two players from start to finish.  

The program is able to:
- detect the legality of the moves that the users try to make
- evaluate and return to the user all the available moves a piece can make
- decree conditions of check, stall and checkmate

## Motivation
*ChiLL* was born as a fun proof of concept while studying logic programming with Prolog.  
It's known that there are better languages for this kind of application, but I found it cool that, at times, coding in logic language resembled writing a chess instruction manual.


## How to play: an example
TODO.


## API Reference
Useful queries for the interaction with the chessboard are listed below: the user can either request the status of the game, get the possible moves for any of its pieces, and actually moving them.  

- **turn(-Color)**  
  Indicates the current turn: *Color* can be ``black`` or ``white``.  

- **result(-R)**  
  Asks for the current game status: *R* can be ``white_won`` or ``black_won`` (by checkmate), ``draw`` (by stalemate), ``white_in_check``, ``black_in_check``, or ``nothing``.  

- **chessboard(-Cells_list)**  
  Retrieves all the chessboard cells in a list, following the format ``cell(point(X,Y),Piece)`` for every item of the list.  

- **chessboard_compact(-Compact\_list)**  
  Retrieves all the chessboard cells in a list, following the compact format ``[X,Y,Piece]`` for every item of the list.  

- **last_moved(-Cells\_list)**  
  Retrieves a list containing the cells involved in the last movement, following the format ``cell(point(X,Y),piece)`` for every list item.

- **last_moved_compact(-Compact_list)**  
  Retrieves a list containing the cells involved in the last movement, following the compact format ``[X,Y,piece]`` for every list item.

- **available_moves(point(+X0,+Y0), -Points\_list)**  
  Retrieves a list containing all the cells to which the (eventual) piece residing in ``point(X0,Y0)`` can move to.  
  List items are presented in the format ``(point(X,Y)``.

- **available_moves_compact([+X0,+Y0], -Compact\_list)**  
  Retrieves a list containing all the cells to which the (eventual) piece residing in ``point(X0,Y0)`` can move to.  
  List items are presented in the compact format ``[X,Y]``.

- **do_move(+Piece, +P0, +P)**  
  Move the *Piece*, if possible, from ``P0`` to ``P``, where both ``P0`` and ``P`` are represented as ``point(X,Y)``.  
  As a consequence of this move, the board will be updated and the turn will be changed.  
  Use this predicate for every move except for: promotions, short castling and long castling.

- **do_move_and_promote(+Piece, +P0, +P, +Promoted_piece)**  
  Move the *Piece*, if possible, from ``P0`` to ``P`` and then promote to the ``Promoted_piece``.  
  Also in this case both ``P0`` and ``P`` must be represented as ``point(X,Y)``.  
  As a consequence of this move, the board will be updated and the turn will be changed.  
  Use this predicate only when a ``pawn`` is reaching the final row and, therefore, must promote.

- **do_short_castle(+King, +P0)**  
  execute, if possible, a short castle for the ``King`` in ``P0``, where ``P0`` is represented as ``point(X,Y)``.  
  As a consequence, the position of both the king and the corresponding rook will be updated on the board and the turn will be changed.

- **do_long_castle(+King, +P0)**  
  Execute, if possible, a long castle for the ``King`` in ``P0``, where ``P0`` is represented as ``point(X,Y)``.  
  As a consequence, the position of both the king and the corresponding rook will be updated on the board, and the turn will be changed.  

Chess pieces are represented by a two-letter code, where the first one represents its color and the second one its role:
- ``wp`` and ``bp`` for White and Black Pawns
- ``wk`` and ``bk`` for White and Black Kings
- ``wq`` and ``bq`` for White and Black Queens
- ``wn`` and ``bn`` for White and Black kNights
- ``wb`` and ``bb`` for White and Black Bishops
- ``wr`` and ``br`` for White and Black Rooks

Notice how the predicates to do the moves include both the ``Piece`` to move and its starting position ``P0``.  
It would have been possible to create an API that omitted the ``Piece``, since it can be derived from the content of the ``cell`` relative to the point ``P0``; however, it was decided to include this redundancy in the requests to make sure that the piece you want to move is actually in ``P0`` when the request is processed.  

**Note on the coordinates convention**: the position of every cell is defined by a pair of zero/based coordinates.  
Since pieces location are usually described on a chessboard using A-H letters for the abscissa and 1-8 for the ordinate, a piece positioned in ``A3`` would be associated with ``point(0,4)`` within our reference system.

## Things I should have done differently
The program is somewhat slow because of the heavy use of ``Retract``/``Assert`` commands while simulating all the possible moves... in the future it would be much better and much faster to simulate possible chessboard combinations in other ways.

## Contribute
Anyone that wants to contribute can contact me or make pull requests.  
At the moment the only move not executable yet is the **en passant** because I lost interest in the project before trying to implement it.  
Another thing that may be useful to implement is a layer that converts standard chess coordinates (eg: *A1* -> *point(0,0)*).
