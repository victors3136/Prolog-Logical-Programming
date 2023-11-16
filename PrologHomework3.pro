% A set of n points in a plane (represented using its coordinates) are
% given. Write a predicate to determine all subsets of collinear points


% slope([Z: Int,T: Int]: Cartesian Coords of a Point, [U: Int, V: Int]:
% Cartesian Coords of a Point, S: Slope of line determined by the 2
% points)
% deterministic for (i,i,o) and (i,i,i)
% will yield a 0 division error if Z = U
% should only be used when [Z,T] and [U,V] are not on on a line
% orthogonal to xOy
slope([Z, T], [U, V], S) :-
    Numerator is 1.0 * (V - T),
    Denominator is 1.0 * (U - Z),
    not(Denominator =:= 0),
    S is (Numerator / Denominator).
% collinear (P1: Cartesian Coords of a Point given as an Int List,
            %P2: Cartesian Coords of a Point given as an Int List,
            %P3: Cartesian Coords of a Point given as an Int List)
% deterministic for (i,i,i)
collinear([X,_],[X,_],[X,_]):-!.
collinear([_,Y],[_,Y],[_,Y]):-!.
collinear([X, Y], [Z, T], [U, V]) :-
    slope([Z, T], [U, V], S),
    Y =:= (S * X - S * Z + T).


% 'in' is just a deterministic version of candidte :)
% E: any
% L: any list, either homogenous or heterogenous
% Used to check if an element is part of a List.
% deterministic for (i,i) and (o,i)
%                             | false , if list is empty
% in(Elem,  {L1,L2,...,Ln}) = | true , if Elem is L1
%                             | in(Elem, {L2,L3,...,Ln}, else
in(E, [E|_]):- !.
in(E, [_ | T]) :- in(E, T).

% 'candidate' is ideentical to 'in' except that it does not stop at
% the first element in the (o,i) case
candidate(H, [H | _]).
candidate(E, [_ | T]) :- candidate(E, T).

% A list is a final result to the 'solution' predcate if it is the right
% size, or rather
% a list reaches the required length while respecting the constraints ->
% the list is a solution to the problem
% final(LLen: Int, RLen: Int)
% deterministic for (i,i)
final(LLen, RLen):- LLen=:=RLen.
% Constraints determines if, by adding Point to the List we keep List
% able to lead to a solution for the problem
% constraints(Point: Cartesian Coords of a Point, List : List of points
% sorted in decreasing distance from the origin, LenList: Int, ReqLen:
% Int)
% deterministic for (i,i,i,i)
%
%                List has length less than the required length |
%  Point is further from the Origin than any point in the List |
%                                                List is empty |=> List keeps its properties w Point is added to it
%                                 OR List has only one element |
%          OR Point is collinear to all the points in the List |
constraints(Point, List, LenList, ReqLen) :-
    LenList < ReqLen,
    not(in(Point,List)),
    furthest_away_from_origin(Point, List),
    collinear_or_else(Point, List).
% Set -> distinct points
% collinear_or_else(P: Cartesian Coords of a Point, L : List of Point
% Cartesian Coords)
% deterministic for (i,i,i)
% collinear_or_else(P,{L1,L2,...,Ln}) =| true, if n is 0 or 1
%                                      | collinear(P,L1,L2), else
collinear_or_else(_, []):-!.
collinear_or_else(_, [_]):-!.
collinear_or_else(P, [X, Y | _]) :- collinear(P, X, Y).

% solution(CL: List of Point Cart Coords, Solution: List of Point Card
% Coords, CLength: Int, SolLen: Int, Input: List of Point Card Coords)
% nondeterministic for (i,o,i,i,i)
% Solution to problem = | CurrentList, if final
%                       | Solution(P U CurrentList, CLength +1), if P is
%                       a candidate and by adding P to List, the
%                       constraints are still satisfiable
%
solution(CurrentList, CurrentList, CurrentLength, SolLength, _):-
    final(CurrentLength, SolLength),
    writeln(CurrentList).
solution(CurrentList, Solution, CurrentLength, SolLength, Input):-
    candidate(Point, Input),
    constraints(Point, CurrentList, CurrentLength, SolLength),
    NextLength is CurrentLength +1,
    solution([Point|CurrentList],Solution,NextLength, SolLength, Input).

% distance_from_origin(P: Cart Coords of a Point, D: Float)
% deterministic for (i,o)
distance_from_origin([X,Y], D):- D is sqrt(X*X+Y*Y).

% furthest_away_from_origin(P: Cart Coords of a Point, List: List of cart
% deterministic for (i,i)
% coords of points ordered in decreasing distance from the origin
% f_a_f_o(P,{L1,L2,...,Ln}) = | true, if L is empty
%                             | distance_from_origin(P) >= distance_from_origin(L1), else
%                             >= in order to accomodate for obviously colinear combinations of points like [0,-2], [0,2], [0,0]
furthest_away_from_origin(_, []):-!.
furthest_away_from_origin([X,Y],[[Z,T]|_]):-
    distance_from_origin([X,Y],D1),
    distance_from_origin([Z,T],D2),
    D1>=D2.
% wrapper for solution predicate
% nondetermnistic for (i,o)
% Input: List of Point Coords
% Solution: List of Point Coords
problem(Input,Solution):-
    solution([], Solution, 0,3,Input).
p3(Input, Result):-
    findall(X,problem(Input, X),Result).

main:-
    test_slope,
    test_collinear,
    test_in,
    test_candidate,
    test_constraints,
    test_collinear_or_else,
    test_furthest_away_from_origin,
    test_problem.
test_slope:-
    slope([1,1],[2,2],1.0),
    slope([0,0],[1,2], 2.0),
    slope([1,2], [2,3], 1.0).
test_collinear:-
    collinear([1,1],[1,2],[1,3]),
    collinear([1,1],[2,1],[3,1]),
    collinear([0,1],[2,2],[4,3]),
    not(collinear([1,1],[3,2],[0,2])).
test_in:-
    in(1,[1,2,3]),
    in(e,[1,e,5,b]),
    in([1,2],[1,2,3,[1,2],[],[7,1]]),
    in([1,2],[[2,2],[3,3],[1,2],[4,7]]),
    not(in([1,2],[[2,2],[3,3],[4,7]])).
test_candidate:-
    candidate(1,[1,2,3]),
    candidate(e,[1,e,5,b]),
    candidate([1,2],[1,2,3,[1,2],[],[7,1]]),
    candidate([1,2],[[2,2],[3,3],[1,2],[4,7]]),
    not(candidate([1,2],[[2,2],[3,3],[4,7]])).
test_constraints:-
    constraints([4,4],[[3,4]], 1, 3),
    constraints([4,4],[[3,2],[2,0]], 2, 3),
    not(constraints([4,4],[[3,2],[2,0]], 2, 2)),
    constraints([5,5],[[4,4],[3,3],[2,2],[1,1],[0,0]],5 , 6).
test_collinear_or_else:-
    collinear_or_else([1,1],[]),
    collinear_or_else([0,0], [[1,1]]),
    collinear_or_else([1,1],[[1,2],[1,3]]),
    collinear_or_else([1,1],[[2,1],[3,1]]),
    collinear_or_else([0,1],[[2,2],[4,3]]),
    not(collinear_or_else([1,1],[[3,2],[0,2]])).
test_distance_from_origin:-
    distance_from_origin([0,0],0.0),
    distance_from_origin([3,4],5.0),
    distance_from_origin([5,12],13.0).
test_furthest_away_from_origin:-
	furthest_away_from_origin([0,0],[]),
	furthest_away_from_origin([1,1],[[0,0]]),
	furthest_away_from_origin([-2,1], [[2,1],[1,1],[0,0]]).
test_problem:-
    problem([[1,1],[2,3],[2,2],[4,7],[5,5],[3,3]],[[3,3],[2,2],[1,1]]),
    problem([[1,1],[2,3],[2,2],[4,7],[5,5],[3,3]],[[5,5],[3,3],[1,1]]),
    problem([[1,0],[6,5],[2,3],[2,-1],[2,2],[4,2],[4,7],[5,5],[3,3]],[[6,5],[4,2],[2,-1]]).
