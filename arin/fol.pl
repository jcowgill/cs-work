% Main solving rule
solvable(X) :- goal(X); (transition(X, Y), solvable(Y)).

% Input facts
transition(a, b).
transition(a, c).
transition(b, d).
transition(c, d).
transition(d, e).
goal(e).
