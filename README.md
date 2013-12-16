menhir
======

Unofficial repository. Experimentations around menhir parser generator
Features are implemented in various branches.


master
------

Import of official source code, maybe with cosmetic patches, cleanups…


partial-order
-------------

STATUS: Done, fully working, need documentation.

Yacc commands to introduce precedences to tokens (```%nonassoc```, ```%left``` and ```%right```) impose a total-ordering.
This arbitrarily defines an order over otherwise unrelated tokens, thereby forcing you to overspecify the grammar.

This branch has a new command, ```%priorities```, which allows to specify a partial ordering over tokens.

Usage:

    %priorities PLUS MINUS MULT DIV UMINUS

Means PLUS < MINUS < MULT < DIV < UMINUS, and nothing more.
Cycles are rejected. Tokens not specified in a %priorities declaration will keep the usual Yacc ordering.
Warning about unused precedence declaration are not emitted for partially-ordered tokens.


experimental
------------

STATUS: Done, fully working (though tested only in a small set of scenarios), need documentation, User-API may be reworked.

Extensions apply only to ```--table``` backend.

*** Inversion of control / step-by-step interface

Flag: ```--stepwise```.

Rather than having the parser invoke the lexer, the parser exposes a step function you feed with token. 

*** Functional interface

Flag: ```--stepwise```.

The parser state is a purely functional data structure: you can keep previous states to resume parse from arbitrary points, with a small memory overhead.

*** Stack introspection

Flag: ```--typed-values```.

Frames of the parser stack are typed and accessible -- you can read already reduced values and shifted tokens.

*** Feed non-terminal

Flag: ```--feed-nonterminal```.
Every non-terminal come with a special rule directly reducing the 

experimental-query
------------------
