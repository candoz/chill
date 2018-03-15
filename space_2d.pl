%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         Point representation in 2d space with integer coordinates          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%point(?X, ?Y)
point(X, Y) :-
  (integer(X); var(X)),
  (integer(Y); var(Y)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    Projection of the vector point(X0,Y0) -> point(X,Y) on N,S,W,E axes     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%projection_north(+point(X0,Y0), +point(X,Y), -Projection)
projection_north(point(_,Y0), point(_,Y), Projection) :- nonvar(Y0), nonvar(Y), Projection is Y - Y0, !.  % green cut
%projection_north(+point(X0,Y0), -point(X,Y), +Projection)
projection_north(point(_,Y0), point(_,Y), Projection) :- nonvar(Y0), nonvar(Projection), Y is Y0 + Projection.

%projection_south(+point(X0,Y0), +point(X,Y), -Projection)
projection_south(point(_,Y0), point(_,Y), Projection) :- nonvar(Y0), nonvar(Y), Projection is Y0 - Y, !.  % green cut
%projection_south(+point(X0,Y0), -point(X,Y), +Projection)
projection_south(point(_,Y0), point(_,Y), Projection) :- nonvar(Y0), nonvar(Projection), Y is Y0 - Projection.

%projection_east(+point(X0,Y0), +point(X,Y), -Projection)
projection_east(point(X0,_), point(X,_), Projection) :- nonvar(X0), nonvar(X), Projection is X - X0, !.  % green cut
%projection_east(+point(X0,Y0), -point(X,Y), +Projection)
projection_east(point(X0,_), point(X,_), Projection) :- nonvar(X0), nonvar(Projection), X is X0 + Projection.

%projection_west(+point(X0,Y0), +point(X,Y), -Projection)
projection_west(point(X0,_), point(X,_), Projection) :- nonvar(X0), nonvar(X), Projection is X0 - X, !.  % green cut
%projection_west(+point(X0,Y0), -point(X,Y), +Projection)
projection_west(point(X0,_), point(X,_), Projection) :- nonvar(X0), nonvar(Projection), X is X0 - Projection.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       Moving some steps towards one particular direction (N,S,W,E)         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NB: By design, "Steps" > 0.
%      At least one between "P" and "Steps" must be istantiated.


%steps_north(+P0, ?P, ?Steps)  
steps_north(P0, P, Steps) :- 
  projection_north(P0, P, Steps),
  projection_east(P0, P, 0),
  Steps > 0.

%steps_south(+P0, ?P, ?Steps)
steps_south(P0, P, Steps) :- 
  projection_south(P0, P, Steps),
  projection_east(P0, P, 0),
  Steps > 0.

%steps_east(+P0, ?P, ?Steps)
steps_east(P0, P, Steps) :- 
  projection_east(P0, P, Steps),
  projection_north(P0, P, 0),
  Steps > 0.

%steps_west(+P0, ?P, ?Steps)
steps_west(P0, P, Steps) :- 
  projection_west(P0, P, Steps),
  projection_north(P0, P, 0),
  Steps > 0.


%steps_north_east(+P0, ?P, ?Steps)
steps_north_east(P0, P, Steps) :-
  projection_north(P0, P, Steps),
  projection_east(P0, P, Steps),
  Steps > 0.

%steps_north_west(+P0, ?P, ?Steps)
steps_north_west(P0, P, Steps) :-
  projection_north(P0, P, Steps),
  projection_west(P0, P, Steps),
  Steps > 0.

%steps_south_east(+P0, ?P, ?Steps)
steps_south_east(P0, P, Steps) :-
  projection_south(P0, P, Steps),
  projection_east(P0, P, Steps),
  Steps > 0.

%steps_south_west(+P0, ?P, ?Steps)
steps_south_west(P0, P, Steps) :-
  projection_south(P0, P, Steps),
  projection_west(P0, P, Steps),
  Steps > 0.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  Relevant alignements between two points                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%aligned_north(+P0, +P)
aligned_north(P0, P) :- steps_north(P0, P, _).

%aligned_south(+P0, +P)
aligned_south(P0, P) :- steps_south(P0, P, _).

%aligned_east(+P0, +P)
aligned_east(P0, P) :- steps_east(P0, P, _).

%aligned_west(+P0, +P)
aligned_west(P0, P) :- steps_west(P0, P, _).


%aligned_north_east(+P0, +P)
aligned_north_east(P0, P) :- steps_north_east(P0, P, _).

%aligned_north_west(+P0, +P)
aligned_north_west(P0, P) :- steps_north_west(P0, P, _).

%aligned_east(point(+P0, +P)
aligned_south_east(P0, P) :- steps_south_east(P0, P, _).

%aligned_west(point(+P0, +P)
aligned_south_west(P0, P) :- steps_south_west(P0, P, _).


%aligned_vertically(+P0, +P)
aligned_vertically(P0, P) :- aligned_north(P0, P), !.  % green cut
aligned_vertically(P0, P) :- aligned_south(P0, P).

%aligned_horizontally(+P0, +P)
aligned_horizontally(P0, P) :- aligned_east(P0, P), !.  % green cut
aligned_horizontally(P0, P) :- aligned_west(P0, P).

%aligned_main_diagonal(+P0, +P)
aligned_main_diagonal(P0, P) :- aligned_north_west(P0, P), !.  % green cut
aligned_main_diagonal(P0, P) :- aligned_south_east(P0, P).

%aligned_anti_diagonal(+P0, +P)
aligned_anti_diagonal(P0, P) :- aligned_north_east(P0, P), !.  % green cut
aligned_anti_diagonal(P0, P) :- aligned_south_west(P0, P).


%aligned_axis(+P0, +P)
aligned_axis(P0, P) :- aligned_vertically(P0, P), !.  % green cut
aligned_axis(P0, P) :- aligned_horizontally(P0, P).

%aligned_diagonally(+P0, +P)
aligned_diagonally(P0, P) :- aligned_main_diagonal(P0, P), !.  % green cut
aligned_diagonally(P0, P) :- aligned_anti_diagonal(P0, P).


%aligned(+P0, +P)
aligned(P0, P) :- aligned_straight(P0, P), !.  % green cut
aligned(P0, P) :- aligned_diagonal(P0, P).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%               Lists of points between two (aligned) points                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    

%in_between_north(point(+X0,+Y0), point(+X,+Y), -Points)
in_between_north(point(X0,Y0), point(X0,Y), [point(X0,Yi)]) :- Yi is Y0 + 1, Yi is Y - 1, !.  % green cut
in_between_north(point(X0,Y0), point(X0,Y), [point(X0,Yi) | Other_points]) :- 
  Yi is Y0 + 1, Yi < Y,
  in_between_north(point(X0,Yi), point(X0,Y), Other_points).

%in_between_south(point(+X0,+Y0), point(+X,+Y), -Points)
in_between_south(point(X0,Y0), point(X0,Y), [point(X0,Yi)]) :- Yi is Y0 - 1, Yi is Y + 1, !.  % green cut
in_between_south(point(X0,Y0), point(X0,Y), [point(X0,Yi) | Other_points]) :- 
  Yi is Y0 - 1, Yi > Y,
  in_between_south(point(X0,Yi), point(X0,Y), Other_points).

%in_between_east(point(+X0,+Y0), point(+X,+Y), -Points)
in_between_east(point(X0,Y0), point(X,Y0), [point(Xi,Y0)]) :- Xi is X0 + 1, Xi is X - 1, !.  % green cut
in_between_east(point(X0,Y0), point(X,Y0), [point(Xi,Y0) | Other_points]) :- 
  Xi is X0 + 1, Xi < X,
  in_between_east(point(Xi,Y0), point(X,Y0), Other_points).

%in_between_west(point(+X0,+Y0), point(+X,+Y), -Points)
in_between_west(point(X0,Y0), point(X,Y0), [point(Xi,Y0)]) :- Xi is X0 - 1, Xi is X + 1, !.  % green cut
in_between_west(point(X0,Y0), point(X,Y0), [point(Xi,Y0) | Other_points]) :- 
  Xi is X0 - 1, Xi > X,
  in_between_west(point(Xi,Y0), point(X,Y0), Other_points).


%in_between_north_east(point(+X0,+Y0), point(+X,+Y), -Points)
in_between_north_east(point(X0,Y0), point(X,Y), [point(Xi,Yi)]) :- 
  Xi is X0 + 1, Xi is X - 1,
  Yi is Y0 + 1, Yi is Y - 1, !.  % green cut
in_between_north_east(point(X0,Y0), point(X,Y), [point(Xi,Yi) | Other_points]) :- 
  Xi is X0 + 1, Xi < X,
  Yi is Y0 + 1, Yi < Y,
  in_between_north_east(point(Xi,Yi), point(X,Y), Other_points).

%in_between_north_west(point(+X0,+Y0), point(+X,+Y), -Points)
in_between_north_west(point(X0,Y0), point(X,Y), [point(Xi,Yi)]) :- 
  Xi is X0 - 1, Xi is X + 1,
  Yi is Y0 + 1, Yi is Y - 1, !.  % green cut
in_between_north_west(point(X0,Y0), point(X,Y), [point(Xi,Yi) | Other_points]) :- 
  Xi is X0 - 1, Xi > X,
  Yi is Y0 + 1, Yi < Y,
  in_between_north_west(point(Xi,Yi), point(X,Y), Other_points).

%in_between_south_east(point(+X0,+Y0), point(+X,+Y), -Points)
in_between_south_east(point(X0,Y0), point(X,Y), [point(Xi,Yi)]) :- 
  Xi is X0 + 1, Xi is X - 1,
  Yi is Y0 - 1, Yi is Y + 1, !.  % green cut
in_between_south_east(point(X0,Y0), point(X,Y), [point(Xi,Yi) | Other_points]) :- 
  Xi is X0 + 1, Xi < X,
  Yi is Y0 - 1, Yi > Y,
  in_between_south_east(point(Xi,Yi), point(X,Y), Other_points).

%in_between_south_west(point(+X0,+Y0), point(+X,+Y), -Points)
in_between_south_west(point(X0,Y0), point(X,Y), [point(Xi,Yi)]) :- 
  Xi is X0 - 1, Xi is X + 1,
  Yi is Y0 - 1, Yi is Y + 1, !.  % green cut
in_between_south_west(point(X0,Y0), point(X,Y), [point(Xi,Yi) | Other_points]) :- 
  Xi is X0 - 1, Xi > X,
  Yi is Y0 - 1, Yi > Y,
  in_between_south_west(point(Xi,Yi), point(X,Y), Other_points).


%in_between(+P0, +P, -Points)
in_between(P0, P, Points) :- in_between_north(P0, P, Points), !.       % green cut
in_between(P0, P, Points) :- in_between_south(P0, P, Points), !.       % green cut
in_between(P0, P, Points) :- in_between_east(P0, P, Points), !.        % green cut
in_between(P0, P, Points) :- in_between_west(P0, P, Points), !.        % green cut
in_between(P0, P, Points) :- in_between_north_east(P0, P, Points), !.  % green cut
in_between(P0, P, Points) :- in_between_north_west(P0, P, Points), !.  % green cut
in_between(P0, P, Points) :- in_between_south_east(P0, P, Points), !.  % green cut
in_between(P0, P, Points) :- in_between_south_west(P0, P, Points).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Miscellaneous                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%adjacent(+point(X0,Y0), ?point(X,Y))
adjacent(point(X0,Y0), point(X,Y)) :-
  (X is X0 - 1; X is X0; X is X0 + 1),
  (Y is Y0 - 1; Y is Y0; Y is Y0 + 1),
  not (X = X0, Y = Y0).  % comment out this subgoal to include the soultion where point(X,Y) is the same as point(X0,Y0)


%l_pattern(+point(X0,Y0), ?point(X,Y))
l_pattern(point(X0,Y0), point(X,Y)) :-
  (
    (X is X0 + 2; X is X0 - 2),
    (Y is Y0 + 1; Y is Y0 - 1)
  );
  (
    (X is X0 + 1; X is X0 - 1),
    (Y is Y0 + 2; Y is Y0 - 2)
  ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Obsolete                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% In this (previous) version the 2nd point should have been mandatory instantiated: I found that to be an unnecessary limit. 
%adjacent_old(+P1, +P2)
adjacent_old(P1, P2) :-
  north_projection(P1, P2, N_Proj), member(N_Proj, [-1, 0, 1]),
  east_projection(P1, P2, E_Proj), member(E_Proj, [-1, 0, 1]),
  not (P1 = P2).


%% An equivalent (but slightly less efficient) version of l_pattern/2.
%l_pattern_old(+P0, ?P)
l_pattern_old(P0, P) :-
  (
    (steps_north(P0, P1, 2); steps_south(P0, P1, 2)),
    (steps_east(P1, P, 1); steps_west(P1, P, 1))
  );
  (
    (steps_north(P0, P1, 1); steps_south(P0, P1, 1)),
    (steps_east(P1, P, 2); steps_west(P1, P, 2))
  ).
