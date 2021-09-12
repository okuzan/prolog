div_mod( M , N , Q , R ) :-
  M > 0,
  N > 0,
  mod_help( M , N , 0 , Q , R ).

mod_help( M , N , Q , Q , M ) :-   M < N.
mod_help( M , N , T , Q , R ) :-   
  M >= N,                       
  T1 is T+1,                     
  M1 is M-N,   
  mod_help( M1 , N , T1 , Q , R).

fib(0, 0).
fib(1, 1).
fib(N,Result) :- fib_help(N,0,1,Result).

fib_help(0,N,_,N).
fib_help(N, P1, P2,Result):-
   N>0,
   NP2 is P1+P2,
   N1 is N-1,
   fib_help(N1,P2,NP2,Result).

euclid_algo(A, 0, C) :- C is A.
euclid_algo(A, B, C) :- B > A, euclid_algo(B, A, C).
euclid_algo(A, B, C) :- X is A mod B, euclid_algo(B, X, C).
gcd(A, B, C) :- euclid_algo(A, B, C). 

powers(_, 0, 1).
powers(A, B, C) :- B1 is B-1, 
                   G is A*C,
                   powers(A, B1, G).

powers_log(A, B, C) :- log(C, A, B),!.

log(1, _, 0).
log(N, B, Res):-
    N > 1,
    N1 is N/B,
    log(N1, B, A),
    Res is A + 1.

recur(N, Res) :- recur_help(N, 0, 1, 0, Res).

recur_help(N, N, _, Q, Q).
recur_help(N, C, L, Q, Res) :- 
C1 is C+1,
L1 is L/C1,
Q1 is Q + L1,
recur_help(N, C1, L1, Q1, Res).

prime_factor(N, Res) :-
    prime_factor(N, 2, Res).

prime_factor(N, Res, Res) :-
    0 is N mod Res.
prime_factor(N, Res, R) :-
    Res < N,
    (0 is N mod Res
    -> (N1 is N/Res, prime_factor(N1, Res, R))
    ;  (Res1 is Res + 1, prime_factor(N, Res1, R))
    ).