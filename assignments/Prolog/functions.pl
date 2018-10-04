not(X) :- X, !, fail.
not(_).

mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.


degree_to_radians( degmin( Degree, _), Radians ) :-
   Radians is ((Degree * pi)/ (180)).

 
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
    Dlon is Lon2 - Lon1,
    Dlat is Lat2 - Lat1,
    A is sin( Dlat / 2 ) ** 2 
        + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
    Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
    Distance is Dist * 3961.

distance(Airp1, Airp2, Distance) :-
   airport(Airp1, _, Lat1, Lon1),
   airport(Airp2, _, Lat2, Lon2),
   degree_to_radians(Lat1, Lat1_rad),
   degree_to_radians(Lon1, Lon1_rad),
   degree_to_radians(Lat2, Lat2_rad),
   degree_to_radians(Lon2, Lon2_rad),
   haversine_radians(Lat1_rad, Lon1_rad, Lat2_rad, Lon2_rad, Distance).

to_hour(time(Hour,Minute),TotalHour) :-
    TotalHour is Hour +Minute /60.


print_digits( Digits ) :-
   Digits < 10, 
   print( 0 ), print( Digits ).

print_digits( Digits ) :-
   Digits >= 10,
   print( Digits ).


print_time(TotalHour):-
    Totaltime is floor( TotalHour *60),
    Hour is Totaltime //60,
    Minute is Totaltime mod 60,
    print_digits(Hour),
    print(':'),
    print_digits(Minute).

arrive_time(flight(Airp1,Airp2,time(Hour,Minute)), ArrivalTime) :-
    fly_time(Airp1, Airp2, FlyingTime),
    to_hour(time(Hour, Minute),DepartingTime),
    ArrivalTime is FlyingTime + DepartingTime.

fly_time(Airp1, Airp2, FlyingTime) :-
    distance(Airp1, Airp2, DistanceMiles),
    FlyingTime is DistanceMiles/500.


writepath([]).
writepath([flight(Depart, Arrive, DTime)|Path]) :-
    airport(Depart, DepartName, _, _), 
    airport(Arrive, ArriveName, _, _),
    write('depart  '), write(Depart), write('  '), write(DepartName),
    write('  '), to_hour(DTime, DepartingTime), 
    print_time(DepartingTime), nl,
    write('arrive  '), write(Arrive), write('  '), write(ArriveName),
    write('  '), 
    arrive_time(flight(Depart, Arrive, DTime), ArrivalTime),
    print_time(ArrivalTime), nl, !,
    writepath(Path).
writepath(_).

listpath(Node, End, [flight(Node, Next, DepartingTime)|Path]) :-
  flight(Node, Next, DepartingTime),
  listpath(Next, End, [flight(Node, Next, DepartingTime)], Path).

listpath(Node, Node, _, [Node]).
listpath(Node, End, [flight(PrevD, PrevA, PrevT)|Tried],
[flight(Node, Next, DepartingTime)|Path]) :-
   flight(Node, Next, DepartingTime),
   arrive_time(flight(Node, Next, DepartingTime), ArrivalTime),
   not(member(flight(Node, Next, DepartingTime), Tried)),
   arrive_time(flight(PrevD, PrevA, PrevT), PrevArrival),
   to_hour(DepartingTime, CurrentDepart),
   Transfer is CurrentDepart - PrevArrival,
   (CurrentDepart > PrevArrival),
   (ArrivalTime < 24),
   (Transfer > 0.5),
     listpath(Next, End,
     [flight(Node, Next, DepartingTime)|Tried], Path).

fly(Depart, Arrive) :-
    airport(Depart, _, _, _), airport(Arrive, _, _, _),
    listpath(Depart, Arrive, Path), !,
    writepath(Path), true.

fly(Depart, Depart) :- write('Error: two airports are the same.'),
 nl, !, fail.
fly(Depart, Arrive) :- 
    airport(Depart, _, _, _), airport(Arrive, _, _, _),
    write('Error: Invalid Path.'), nl, !, fail.

fly(_,_) :- write('Error: Invalid Flight.'), nl, !, fail.
