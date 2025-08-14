# Todos

## Lichessserver

### Chess Move Tree

- we can build a tree from the moves out of a pgn file
- now we need to be able to move along the tree
  - [x] because the chess course files do not have to be complete but I want it to be complete until
    the 8th move or so I will have to save the chess courses in a file in some text format I want
    to use S-Expressions
  - [x] I will also have to merge the move tree from the different course parts of white and black
- merging works but there is a problem with reoccurring positions we basically do not have a strict
  tree we have an acyclic graph where some nodes can have two "parents" >> transposition

### Oh Oh TODOs limitations for san parsing

- En passant capture moves
- san for moves that a disambiguous because of game semantics (e.g. pin)
- and it does not work I use a js lib for that in the frontend

