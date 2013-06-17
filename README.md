unfolder
========

Experimental unfolder for full featured functional programs

The project aims at creating a 'toolkit' that can be used to unfold a 'full featured' functional program (that is, 
programs that have all the features in functional programming which are not present in logic programming):

- Function composition
- Lazy evaluation
- Higher order

The distinction between functional and logic programming is relevant here because although the unfolder is written in
Prolog, it unfolds functional programs. 

Programs to be unfolded are by the moment written in a generic functional language that allows higher order, partially
undefined expressions and handles guarded program rules. Ultimately, the unfolder should admit programs written in a
real language (e.g. Haskell).

* Unfolding
-----------

Unfolding is the process of replacing a function invocation by the definition of that very same function. By repeating
the process as many times as necessary, what we get is a program that has the same meaning as the original 'folded'
but that is simpler than the original one in terms of function composition (a completely unfolded proggram has no
function composition at all but this is not possible in general). That simpler form of the program can be seen as a set
of facts that plainly express the meaning of the original program (its semantics).



