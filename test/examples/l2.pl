q(a, b).
r(b, c).
p(X, X) :- q(X, Z), r(Z, Y).

?- p(U, V).