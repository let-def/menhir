menhir
======

Unofficial repository. Experimentations around menhir parser generator.  
Features are implemented in various branches.


master
------

Import of official source code, maybe with cosmetic patches, cleanups…


partial-order
-------------

*STATUS:* Done, fully working, need documentation.

Yacc commands to introduce precedence (```%nonassoc```, ```%left``` and ```%right```) impose a total-ordering.  
This defines a somewhat arbitrary order over otherwise unrelated tokens, thereby forcing you to overspecify the grammar.  
The new command ```%priorities``` can be used to specify a partial ordering over tokens.

    %priorities PLUS MINUS MULT DIV UMINUS

Means that ```PLUS < MINUS < MULT < DIV < UMINUS```, and nothing more.  
Tokens not specified in a %priorities declaration will keep the usual Yacc ordering.

Cycles are rejected.  
Warning about unused precedence declaration are not emitted for partially-ordered tokens.


experimental
------------

*STATUS:* Done, fully working (though tested only in a small set of scenarios), need documentation, User-API may be reworked.

Extensions apply only to ```--table``` backend.

### Inversion of control / step-by-step interface

Flag: ```--stepwise```.  
Rather than having the parser invokes the lexer, the parser exposes a step function getting a token. 

### Functional interface

Flag: ```--stepwise```.  
The parser state is a purely functional data structure: you can keep previous states to resume parse from arbitrary points, with a small memory overhead.

### Stack introspection

Flag: ```--typed-values```.  
Frames of the parser stack are typed and accessible. You can read reduced values and shifted tokens.

### Feed non-terminal

Flag: ```--feed-nonterminal```.  
Every non-terminal come with a special rule directly reducing the production.

E.g. given:

    %type <ast> expr
    
At the beginning of an expr non-terminal, the user can feed the parser with an
```NT'expr some_ast``` to circumvent normal parsing, manually passing the semantic value.

experimental-query
------------------

*STATUS:* WIP, extends experimental branch.

### Query actions

Given a ```state``` and a ```terminal/token```, get the action that would be
executed by the parser: ```reduce```, ```shift``` or ```fail```.

### Forward reference graph

Get the graph of dependencies between non-terminals (as produced by ```--dot```) from user-code.
