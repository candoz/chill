%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         Point representation in 2d space with integer coordinates          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%point(?X, ?Y)
point(X, Y) :-
  (integer(X); var(X)),
  (integer(Y); var(Y)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%         Projection of the vector (X0,Y0) -> (X,Y) on N/S/W/E axes          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%north_projection(+point(X0,Y0), +point(X,Y), -Projection)
north_projection(point(_,Y0), point(_,Y), Projection) :- nonvar(Y0), nonvar(Y), Projection is Y - Y0, !.  % green cut
%north_projection(+point(X0,Y0), -point(X,Y), +Projection)
north_projection(point(_,Y0), point(_,Y), Projection) :- nonvar(Y0), nonvar(Projection), Y is Y0 + Projection.

%south_projection(+point(X0,Y0), +point(X,Y), -Projection)
south_projection(point(_,Y0), point(_,Y), Projection) :- nonvar(Y0), nonvar(Y), Projection is Y0 - Y, !.  % green cut
%south_projection(+point(X0,Y0), -point(X,Y), +Projection)
south_projection(point(_,Y0), point(_,Y), Projection) :- nonvar(Y0), nonvar(Projection), Y is Y0 - Projection.

%east_projection(+point(X0,Y0), +point(X,Y), -Projection)
east_projection(point(X0,_), point(X,_), Projection) :- nonvar(X0), nonvar(X), Projection is X - X0, !.  % green cut
%east_projection(+point(X0,Y0), -point(X,Y), +Projection)
east_projection(point(X0,_), point(X,_), Projection) :- nonvar(X0), nonvar(Projection), X is X0 + Projection.

%west_projection(+point(X0,Y0), +point(X,Y), -Projection)
west_projection(point(X0,_), point(X,_), Projection) :- nonvar(X0), nonvar(X), Projection is X0 - X, !.  % green cut
%west_projection(+point(X0,Y0), -point(X,Y), +Projection)
west_projection(point(X0,_), point(X,_), Projection) :- nonvar(X0), nonvar(Projection), X is X0 - Projection.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  Relevant alignements between two points                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%aligned_north(+point(X0,Y0), +point(X,Y))
aligned_north(point(X0,Y0), point(X,Y)) :-
  north_projection(point(X0,Y0), point(X,Y), N_Proj), N_Proj > 0,
  east_projection(point(X0,Y0), point(X,Y), 0).

%aligned_south(+point(X0,Y0), +point(X,Y))
aligned_south(point(X0,Y0), point(X,Y)) :-
  north_projection(point(X0,Y0), point(X,Y), N_Proj), N_Proj < 0,
  east_projection(point(X0,Y0), point(X,Y), 0).

%aligned_east(+point(X0,Y0), +point(X,Y))
aligned_east(point(X0,Y0), point(X,Y)) :-
  north_projection(point(X0,Y0), point(X,Y), 0),
  east_projection(point(X0,Y0), point(X,Y), E_Proj), E_Proj > 0.

%aligned_west(+point(X0,Y0), +point(X,Y))
aligned_west(point(X0,Y0), point(X,Y)) :-
  north_projection(point(X0,Y0), point(X,Y), 0),
  east_projection(point(X0,Y0), point(X,Y), E_Proj), E_Proj < 0.


%aligned_north_east(+point(X0,Y0), +point(X,Y))
aligned_north_east(point(X0,Y0), point(X,Y)) :-
  north_projection(point(X0,Y0), point(X,Y), N_Proj), N_Proj > 0,
  east_projection(point(X0,Y0), point(X,Y), N_Proj).

%aligned_north_west(+point(X0,Y0), +point(X,Y))
aligned_north_west(point(X0,Y0), point(X,Y)) :-
  north_projection(point(X0,Y0), point(X,Y), N_Proj), N_Proj > 0,
  west_projection(point(X0,Y0), point(X,Y), N_Proj).

%aligned_east(+point(X0,Y0), +point(X,Y))
aligned_south_east(point(X0,Y0), point(X,Y)) :-
  south_projection(point(X0,Y0), point(X,Y), S_Proj), S_Proj > 0,
  east_projection(point(X0,Y0), point(X,Y), S_Proj).

%aligned_west(+point(X0,Y0), +point(X,Y))
aligned_south_west(point(X0,Y0), point(X,Y)) :-
  south_projection(point(X0,Y0), point(X,Y), S_Proj), S_Proj > 0,
  west_projection(point(X0,Y0), point(X,Y), S_Proj).


%aligned(+point(X0,Y0), +point(X,Y))
aligned(P0, P) :- aligned_north(P0, P).
aligned(P0, P) :- aligned_south(P0, P).
aligned(P0, P) :- aligned_east(P0, P).
aligned(P0, P) :- aligned_west(P0, P).
aligned(P0, P) :- aligned_north_east(P0, P).
aligned(P0, P) :- aligned_north_west(P0, P).
aligned(P0, P) :- aligned_south_east(P0, P).
aligned(P0, P) :- aligned_south_west(P0, P).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            Lists of points between two given starting points:              %%
%%         they must be aligned over one of the 9 axes of a compass           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
%in_between(+point(X0,Y0), +point(X,Y), -Points)
in_between(point(X0,Y0), point(X,Y), Points) :- aligned_north(point(X0,Y0), point(X,Y)), in_between_north(point(X0,Y0), point(X,Y), Points).
in_between(point(X0,Y0), point(X,Y), Points) :- aligned_south(point(X0,Y0), point(X,Y)), in_between_south(point(X0,Y0), point(X,Y), Points).
in_between(point(X0,Y0), point(X,Y), Points) :- aligned_west(point(X0,Y0), point(X,Y)), in_between_west(point(X0,Y0), point(X,Y), Points).
in_between(point(X0,Y0), point(X,Y), Points) :- aligned_east(point(X0,Y0), point(X,Y)), in_between_east(point(X0,Y0), point(X,Y), Points).
in_between(point(X0,Y0), point(X,Y), Points) :- aligned_north_east(point(X0,Y0), point(X,Y)), in_between_north_east(point(X0,Y0), point(X,Y), Points).
in_between(point(X0,Y0), point(X,Y), Points) :- aligned_north_west(point(X0,Y0), point(X,Y)), in_between_north_west(point(X0,Y0), point(X,Y), Points).
in_between(point(X0,Y0), point(X,Y), Points) :- aligned_south_east(point(X0,Y0), point(X,Y)), in_between_south_east(point(X0,Y0), point(X,Y), Points).
in_between(point(X0,Y0), point(X,Y), Points) :- aligned_south_west(point(X0,Y0), point(X,Y)), in_between_south_west(point(X0,Y0), point(X,Y), Points).  


%% NB: the following predicates must be used anly if the two points are aligned in the respective direction!

%in_between_north(+point(X0,Y0), +point(X,Y), -Points)
in_between_north(point(X0,Y0), point(X0,Y), []) :- Y is Y0 + 1, !.  % red cut
in_between_north(point(X0,Y0), point(X0,Y), [point(X0,Yi) | Other_points]) :- 
  %aligned_north(point(X0,Y0), point(X,Y)),  % commented out for efficiency, see the NB above
  Yi is Y0 + 1,
  in_between_north(point(X0,Yi), point(X0,Y), Other_points).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             Miscellaneous                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%adjacent(+point(X0,Y0), ?point(X,Y))
adjacent(point(X0,Y0), point(X,Y)) :-
  (X is X0 - 1; X is X0; X is X0 + 1), 
  (Y is Y0 - 1; Y is Y0; Y is Y0 + 1),
  not (X = X0, Y = Y0).  % comment out this subgoal to include the soultion where (X,Y) is the same point as (X0,Y0)

%% In this (previous) version the 2nd point should have been mandatory instantiated: I found that to be an unnecessary limit. 
%adjacent_old(+point(X1,Y1), +point(X2,Y2))
adjacent_old(point(X1,Y1), point(X2,Y2)) :-
  north_projection(point(X1,Y1), point(X2,Y2), N_Proj), (N_Proj = 0; abs(N_Proj) =:= 1),
  east_projection(point(X1,Y1), point(X2,Y2), E_Proj), (E_Proj = 0; abs(E_Proj) =:= 1),
  (X1 =\= X2; Y1 =\= Y2).  % comment out this subgoal to include the soultion where (X2,Y2) is the same point as (X1,Y1)


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

%% An equivalent (but slightly less efficient) version of l_pattern/2.
%l_pattern_old(+point(X0,Y0), ?point(X,Y))
l_pattern_old(point(X0,Y0), point(X,Y)) :-
  (
    (two_steps_north(point(X0,Y0), point(A,B)); two_steps_south(point(X0,Y0), point(A,B))),
    (one_step_east(point(A,B), point(X,Y)); one_step_west(point(A,B), point(X,Y)))
  );
  (
    (one_step_north(point(X0,Y0), point(A,B)); one_step_south(point(X0,Y0), point(A,B))),
    (two_steps_east(point(A,B), point(X,Y)); two_steps_west(point(A,B), point(X,Y)))
  ).
