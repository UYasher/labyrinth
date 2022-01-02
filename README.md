![Labyrinth](https://raw.githubusercontent.com/UYasher/labyrinth/main/img/title.png)

## Quick Start
To use the main program as an aide during a game of labyrinth, use `stack run` in the terminal.
To use the source files in your project, see `src/Tile.hs` for the implementation of tiles used on the game board,
`src/Board.hs` for how the tiles are arranged and manipulated on a board, `src/GameState.hs` for implementing the
game from a board, and `src/Lib.hs` for an example of how a user-facing program can be created using these abstractions.

## How Labyrinth Works
*For an explanation of the rules of labyrinth without reference to code, see [the rules](https://www.ultraboardgames.com/labyrinth/game-rules.php)*

Labyrinth is a board game where players compete to reach tiles on a board and return to their starting tile before their opponents. What makes this game special is the board.

The board is made up of square tiles (implemented in `src/Tile.hs`). These tiles have one of three `Shape`s (a straight `pipe`, `elbow`, or `tee`), and can be rotated in 90 degree increments (refered to as `north`, `south`, `east`, and `west` and implemented as functions of the `Orientation` type). 

The board (implemented, along with functions to interact with the board, in `src/Board.hs`) is then a 2D array of `Tile`s. A typicaly board is a 7x7 grid of tiles. We will refer to locations on the board by imposing the following grid with coordinates of the form `(y, x)` (which is also the convention followed in the code):
```
  0123456
0 ┌┘┬┌┬┐┐
1 │―├┐┌││
2 ├│├└┬┬┤
3 └┤┐┐└│┐
4 ├┬┴└┤―┤
5 ┘┐┌――│┤
6 └┌┴┤┴│┘
```
For example, `(1,0)` refers to the 1st row, 0th column (which has a `│` tile).

When a player makes a move, they do two things. First, they insert a tile into the board, then they move. Players can insert a tile into any one of 12 locations on the edges of the board. When they insert a tile, it pushes the entire row/column over and pops off the tile on the opposite side. 

```
Inserting │ into this location
 ↓
┌┘┬┌┬┐┐
│―├┐┌││
├│├└┬┬┤
└┤┐┐└│┐
├┬┴└┤―┤
┘┐┌――│┤
└┌┴┤┴│┘
Gives us the board
┌│┬┌┬┐┐
│┘├┐┌││
├―├└┬┬┤
└│┐┐└│┐
├┤┴└┤―┤
┘┬┌――│┤
└┐┴┤┴│┘
and pushes out the tile at the bottom of that column, namely ┌
```

The next player then uses that tile when inserting into the board on their turn. 

After inserting a tile, they can then move their piece to any connected tile. Two tiles are connected if there is a contiguous path between them.
```
In the board
  0123456
0 ┌│┬┌┬┐┐
1 │┘├┐┌││
2 ├―├└┬┬┤
3 └│┐┐└│┐
4 ├┤┴└┤―┤
5 ┘┬┌――│┤
6 └┐┴┤┴│┘

A player on (0,2) could move to any of the following squares:

  0123456
0   ┬   ┐
1   ├┐  │
2   ├└┬┬┤
3     └│ 
4        
5        
6        
```

## Program Input
In order to use this program through a CLI, it helps to have a textual notation for making moves, similar to that used in chess. The logic behind that notation (as well as the representation of the gamestate and of methods to manipulate it) can be found in `src/GameState.hs`.

We can think of each move as an insertion and a position on the board. The insertion mutates the state of the board, while the poistion is the new location of the player on the mutated board. 

Representing a location on the board is simple -- we can just use the `(y, x)` pairs we established before. But how can we represent the insertions?

One way is as tuples of the form `(orientation:: Orientation, isRow :: Bool, rowNum :: 1|3|5, isFront :: Bool)`. 
```
Given a pipe tile,
inserting │ into this location
 ↓
┌┘┬┌┬┐┐
│―├┐┌││
├│├└┬┬┤
└┤┐┐└│┐
├┬┴└┤―┤
┘┐┌――│┤
└┌┴┤┴│┘
would be represented as (north, False, 1, True)


similarly, inserting ― into this location
┌┘┬┌┬┐┐
│―├┐┌││
├│├└┬┬┤
└┤┐┐└│┐
├┬┴└┤―┤
┘┐┌――│┤
└┌┴┤┴│┘
     ↑
would be represented as (east, False, 5, False)
```

The program can be launched in a terminal using `stack run` in the root of this repo.

Then, the program will prompt you for the orientation (a single letter `n`, `e`, `s`, `w`) in which you would like to insert the tile, and then at which loaction (a tuple `(isRow :: Bool, rowNum :: 1|3|5, isFront :: Bool)`).
```
┌┘┬┌┬┐┐
│―├┐┌││
├│├└┬┬┤
└┤┐┐└│┐
├┬┴└┤―┤
┘┐┌――│┤
└┌┴┤┴│┘

How do you want to insert │?: 
n
And where do you want to insert it?: 
(False, 1, True) 
```

## Strategy
However, the program is sophisticated enough not just to manipulate the board, but also to provide strategic advice in the form of one-turn lookahead.

Because players can move large distances if the connected components of the board are large, and they can change the shape of the board to shape the connected components, it is often possible for a player to reach their target tile in a single move. Players almost always want to do this when possible.

Given a the player's current location, and the location of the tile that the player wants to land on, we can do this using `nextMoveCaptures` in `src/GameState.hs`. This is done by searching through all insertions, and then running a dfs to determine reachability. (Reachability here means that the desired tile is now reachable from the player's location on the new board.)

When running the program via `stack run`, that means telling the program we want to insert the tile with help (`h`) and then providing locations on the board. The program then return all the insertions which make the desired move possible, and you can choose whichever one you prefer.

```
┌┘┬┌┬┐┐
│―├┐┌││
├│├└┬┬┤
└┤┐┐└│┐
├┬┴└┤―┤
┘┐┌――│┤
└┌┴┤┴│┘

How do you want to insert │?: 
h
"Okay, let me help you..."
"What current coordinates on the board are you at?:"
(0,0)   
"What are the current coordinates on the board that you want to move to?:"
(5,0) 
"Here are all the ways of moving between those positions:"
...
```

Enjoy playing Labyrinth!