# Assignment 2: Reversi & Queens

In assessment 2, you have some (preferably functional) coding to do, but I've also pre-written a (not pure) UI so
you can see your code in action, doing something.

In this assessment, we're going to use functional programming to write a little classical AI for two games:

* Reversi
* LinkedIn's Queens game

The assessment introduces two things over assessment 1:

* It uses type aliases and extension methods extensively
* The UI uses Scala.js, so you'll need Node.js installed so that the UI can be compiled and so you can run the tests using Node.js as well as on the JVM.

(You won't need to do any JS coding or UI coding; the UI is already written)

## How the code is set-up

The main code is in `src/main/scala` and should be functional/pure if possible

Some unit tests are in `src/test/scala` but there are not tests for everything - you can write new ones to supplement these!

The UI code is in `src/ui` and has been written for you. The UI code includes the directive to use the JS platform. i.e., if
you compile the code and the UI code is compiled, Scala will compile it for JS instead of the JVM. If you exclude the UI code,
when running the tests, though, it'll exclude the directive to use JS as a platform and instead will run on the JVM.

The UI uses **Scala.js**, so you will need Node.js installed, so that it (and your code) can be cross-compiled to JavaScript.

* To compile the code **without** the UI, and run unit tests using the JVM:  
  `scala-cli test . --exclude src/ui`

* To compile the code **with** the UI, and run init test using JS:
  `scala-cli test .`

* To package the code to JavaScript so you can open the UI in a browser
  `scala-cli --power package . -f --js-emit-source-maps`  


If you package the code to JavaScript, it will create two files:

* `./assignmentTwo.ui.main.js`
* `./assignmentTwo.ui.main.js.map`

If you then open the file `index.html`, it will run in the browser. The browser can be a tough environment to debug in, though, so
I recommend doing most of your debugging via the unit tests (writing some where I've not provided them). I have written some
`prettyPrint` functions so that if you do `println` it'll show you the content of Reversi games in an understandable format.

Note also that some of the UI won't work until you have implemented your functions -- `NotImplementedError`s can interrupt the rendering.

If you get a blank page for either exercise, the chances are an exception has been thrown. You can open the developer tools in your browser
to see what it was (or to do println to the broswer console), but I don't recommend trying to do your debugging in the browser. The browser
doesn't know Scala (even with the "sourcemap" generated) and you'll see some odd looking generated variable and class names that could
be confusing.

Let's explain the two games:

## Reversi

[Reversi](https://en.wikipedia.org/wiki/Reversi) is a traditional game played on an 8x8 board.

There are two players, black and white, who take turns placing disks on the board.

The first two moves for each player involve placing a disk in one of the four central squares of the board. For these two moves,
no captures are made. (We're doing the original 1883 rules.) Programming hint: after these moves, there are four pieces on the board.

For all subsequent moves, a move is valid if it captures at least one piece. (See the examples on the wikipedia page, or try playing it online.)
Note that captures can be horizontal, vertical, or diagonal, and a move might capture pieces on multiple axes.

If a player has no valid moves, their turn is skipped.

If neither player has a valid move, the game ends and the player with the most pieces on the board wins.


### Writing the Reversi play code

The code you are writing will be used both by your own functions for working out what the "AI" should play, and also by the UI for letting the 
user play against it.

For this exercise, **the computer should always make the move that leaves it with the most pieces**. i.e., we're not looking extra moves ahead.

Before you get to that, though, you'll find several other methods that you need to implement:

* getting a line of squares travelling in a direction (N, NE, E, SE, S, SW, W, or NW) from a square to the edge of the board
* counting the number of pieces for each player
* determining what pieces would be flipped by placing a piece in a location
* determining whether a move would be valid for a player
* finding all the valid moves for a player
* generating the new state of the game if a player plays in a particular location
* writing your (relatively simple) function for the computer to play the game greedily.

There is also a history of moves. Clicking in this history of moves will ask your simulation to go back in time as if that move had just been played.
You will notice that the UI wants to keep an immutable `Seq[GameState]` rather than just a game state, for this purpose.

### Tests

I've written some tests for some of your functions in `reversiTests.scala`. There aren't tests for everthing, though. The idea is there's enough to
get you started (and so we can mark the first few marks of your code). From there, you get the rest working so we can open your game up in the browser
and play it.

You will probably also find it helpful to write your own unit tests for other aspects as you develop your code.

## LinkedIn Queens

Queens is a game that LinkedIn have introduced. There's a grid of squares, which are in different colours. E.g.

wwgb
wggb
rrrb
bbbb

A number of Queens are placed on the board, with the following rules:

* Each coloured region has exactly one queen
* Each row has eactly one queen
* Each column has exactly one queen
* No queens are touching (there is not a queen in the 8 squares around a queen)

(But note that unlike the "8 queens problem", in this game queens can't attack each other diagonally more than 1 square away.)

Your task is to work out where all the queens must be.

If you'd like to try it out (and don't have a LinkedIn account), try it online here [Archived Queens](https://www.archivedqueens.com/)

I have created some types and functions for you (and a UI). 

In this case

* The colour grid is a `Seq[Seq[Colours]]`. It's a stack of rows. This has been type aliased to `Grid`
* The map of whether something could be a Queen or a Blank is a `Map[(Int, Int), Set[Contents]]`. 

A little like my (long) example doing Einstein's riddle, at each step you're going to find a location where if you set it to a Queen (or a Blank) 
the resulting grid would be invalid, so you can eliminate that possibility.

Your task is to get `map.makeAstep(grid)` to do this, at which point the UI will let you watch it solve the grid.

One harder grid has been included that can't be solved that way; it's the last mark in the assignment.
(To do that one, you'd need to add an extra rule. e.g. that if there are two colours that only exist in two columns, then no other colour in that 
column can have a Queen)


## Marking

The marking is aimed to be able to be done quickly, with the written feedback being
more formative and open-ended.

Functionality: 

* The Reversi tests that I created pass: 5 marks
* The code cross-compiles to JS and some of it can be played at all in the browser: 1 mark
* It appears to play correctly in the browser: 3 marks

* Your Queens solver works in the browser for the simple grids: 3 marks
* Your Queens solver works in the browser for the harder grid: 1 mark

Quality: 

* Overall quality judgment (functional, readable, tidy, concise): 3

Video:

* Video and explanation: 4
