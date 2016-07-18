# Mancala Bot

This is an A.I. that plays Mancala. It uses the minimax algorithm with alpha-beta pruning. It uses a static evaluation function that was derived using a genetic algorithm.


# Usage

Run `Main.lisp` with the common lisp command line tool to play against the computer.

When it is your turn, enter the number corresponding to the pit whose stones you want to distribute. 

```
> clisp Main.lisp


"[ 0 ]( 4 )( 4 )( 4 )( 4 )( 4 )( 4 )[   ]
 [   ]( 4 )( 4 )( 4 )( 4 )( 4 )( 4 )[ 0 ]
Player 0's Turn
" 
"Legal Moves: (0 1 2 3 4 5)" 
"Please enter a move" 4

"[ 0 ]( 4 )( 4 )( 4 )( 4 )( 5 )( 5 )[   ]
 [   ]( 4 )( 4 )( 4 )( 4 )( 0 )( 5 )[ 1 ]
Player 1's Turn
" 
"[ 1 ]( 5 )( 5 )( 5 )( 0 )( 5 )( 5 )[   ]
 [   ]( 4 )( 4 )( 4 )( 4 )( 0 )( 5 )[ 1 ]
Player 1's Turn
" 
"[ 2 ]( 6 )( 6 )( 6 )( 1 )( 0 )( 5 )[   ]
 [   ]( 4 )( 4 )( 4 )( 4 )( 0 )( 5 )[ 1 ]
Player 1's Turn
" 
"[ 3 ]( 7 )( 0 )( 6 )( 1 )( 0 )( 5 )[   ]
 [   ]( 5 )( 5 )( 5 )( 5 )( 0 )( 5 )[ 1 ]
Player 0's Turn
" 
"Legal Moves: (0 1 2 3 5)" 
"Please enter a move"  
```


# To Do's

- [ ] Add difficulty setting
- [ ] Add entry point to run evolution algorithm
- [ ] Add more documentation
