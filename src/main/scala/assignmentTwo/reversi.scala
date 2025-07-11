package assignmentTwo.reversi

import scala.collection.immutable.Queue
import org.scalajs.dom.Position
import scala.annotation.tailrec

/** The two players in Reversi */
enum Player:
    case Black
    case White

    // A convenience method so you can ask who the other player is, e.g. White.opponent is Black
    def opponent = this match {
        case Black => White
        case White => Black
    }

/** A location on the board. Zero-indexed. Location is a type alias for (Int, Int). e.g. (0, 0) is the top-left */
type Location = (Int, Int)

/** The board size is always 8 by 8 */
val boardSize = 8

/** We represent the board with a Map from location to player. Note that it might not have entries for all locations if a piece hasn't been played there. */
type Board = Map[Location, Player]

/** all the different directions a straight line can go in*/
enum Direction(val dx:Int, val dy:Int):
    case North extends Direction(0, -1)
    case NorthEast extends Direction(1, -1)
    case East extends Direction(1, 0)
    case SouthEast extends Direction(1, 1)
    case South extends Direction(0, 1)
    case SouthWest extends Direction(-1, 1)
    case West extends Direction(-1, 0)
    case NorthWest extends Direction(-1, -1)

// A list of all the directions, North, NorthEast, etc.
def allDirections:Seq[Direction] = Direction.values.toSeq    

// Given a staring location, return (in order) all the squares you would encounter
// walking from that start square to the edge of the board. Include the start square itself.
// If the start square is not on the board, return an empty Seq
//
// You'll need to implement this and might find it useful for "playingHereFlips"
// (Because for each direction, you can ask for a walk from location + direction to the edge in that direction)
def walkToEdge(start:Location, d:Direction):Seq[Location] = {
    
    //For a given start location and direction
    //If the start of our list of locations is not on the board we've gone out of bounds, return the locations we've found
    //If not recursively call, new start point is one step in the given direction and the new list of locations is the old list with the old start added to the head
    @tailrec
    def walkToEdgeInner(start:Location, d:Direction, locations:Seq[Location] = Seq()):Seq[Location] = {
        if (!start.isInRange) return locations
        else {
            walkToEdgeInner(start+d, d, start +: locations)
        }
    }
    //The sequence returned from the recursive call is backwards so reverse it before returning it
    walkToEdgeInner(start, d).reverse
}

/** 
 * Defines some methods we can call on Locations.
 */
extension (l:Location) {

    // Lets us add a direction to a location to step in that direction
    // e.g. (1, 1) + Direction.North == (1, 0)
    def +(dir:Direction):Location = 
        val (x, y) = l
        (x + dir.dx, y + dir.dy)

    // Checks whether a square is on the board
    // e.g. (99, 99).isInRange == false
    def isInRange:Boolean = 
        val (x, y) = l
        (0 until boardSize).contains(x) && (0 until boardSize).contains(y)

    // Whether the location l is in the centre squares that are legal for the first 4 moves in Reversi
    def isInCentre:Boolean = 
        val (x, y) = l
        (x == 3 || x == 4) && (y == 3 || y == 4)

    // Let's us say, for instance a1 instead of (0, 7). Note that locations are from the top-left down,
    // but we're used to seeing chess locations written with a1 in the bottom left
    def toChessLocation:String = 
        val (x, y) = l
        "" + "abcdefgh"(x) + (8 - y)

}


/**
  * Defines some methods on Boards
  */
extension (board:Board) {

    /** The number of pieces on the board. You need to implement this */
    def pieces:Int = 
        board.map{case (location, player) => player}.toSeq.length
        

    /** The number of pieces the given player has. You need to implement this */
    def piecesFor(p:Player):Int = 
        val player_tokens = board.map{case (location, player) => player}.toSeq
        player_tokens.count(player_token => player_token == p)

    /**
      * If player p placed a piece in location l, what are the locations it would flip? You need to implement this.
      * 
      * Hint: For each direction, get the locations that walk in that direction while it contains pieces (starting one step in that direction)
      *       if there aren't any of your pieces, you can't capture anything (you've got nothing on the other side)
      *       if there is one of your pieces on that line, take all the pieces up to your first piece on the line
      */
    def playingHereFlips(l:Location, p:Player):Seq[Location] = {
        //generate the sequence of squares from where we intend to place a piece
        //to the end of the board in all directions
        val allWalks = allDirections.map(direction => walkToEdge(l, direction))
        
        /* 
        I'll recursively move thorugh each walk
        If the sequence is empty we've reached the end without encountering one of our own pieces, we flip nothing return empty sequence
        If we encounter an enemy piece added to the sequence of pieces we intend to flip
        If we encounter one of our own pieces, return the sequence of enemy pieces we've passed so far, we will be flipping these. 
        So this will return a sequence of locations that contain pieces we will flip
         */
        @tailrec
        def playingHereFlipsInner(walk:Seq[Location], flipped: Seq[Location] = Seq()):Seq[Location] = {
            walk match
                case Nil => Seq()
                case head :: Nil => {
                    val square_value = board.getOrElse(head, Nil)
                    if (square_value == Nil) Seq()
                    else {
                        if (square_value == p.opponent) Seq()
                        else flipped
                    }
                }
                case head :: tail =>{
                    val square_value = board.getOrElse(head, Nil)
                    if (square_value == Nil) Seq()
                    else {
                        if (square_value == p.opponent) playingHereFlipsInner(tail, head +: flipped)
                        else flipped
                    }
                }
            
            } 

            /* I'll call flatmap on allwalks and pass the walk to my recursive check
            we'll end up with a sequence of sequences, the pieces that will be flipped if we play here. 
            want to flatmap because I want all the pieces in all directions.
            We will then filter this based on length.
            Which potential place to put a counter will flip the most enemy pieces?
             */
            allWalks.flatMap(walk => playingHereFlipsInner(walk.tail))
    }   
        


    // Whether it'd be valid for player p to play in location l (assuming it's their turn). You need to implement this.
    // Hint: If there are fewer than 4 pieces on the board, it is a valid move if it is in one of the centre squares that is not already occupied
    // Hint: Otherwise, it's a valid move if it is empty and flips more than 0 pieces.
    def isValidMove(l:Location, p:Player):Boolean = {
        if (board.pieces < 4) {
            if (board.getOrElse(l, Nil) == Nil && l.isInCentre) true
            else false
        }
        else (board.getOrElse(l, Nil) == Nil && (board.playingHereFlips(l, p).length > 0))
    }
        
        

    // All the moves that are valid from this position for player p. You need to implement this.
    // Hint: Get a list of every location from (0, 0) to (7, 7) and filter it for the ones that are valid
    def allValidMoves(p:Player):Seq[Location] = {
        val columns = (0 to 7)
        val rows = (0 to 7)
        //take row use flatMap, pull out the column value and have it yield the product of the interior function 
        //we'll use map to iterate over every column element in the rows and have it yield a sequence of tuples consisting 
        //of the previously extracted row value with this currently extracted column value
        //this should return a sequence of all the positions in this row, flatMap will then flatten these into one long sequence which is
        //our board
        // columns.flatMap(c => rows.map(r => (c, r)))
        val positions = rows.flatMap(r => columns.map(c => (c, r)))
        positions.filter(position => isValidMove(position, p))
    }

    /** 
     * Returns a board where player p has placed a piece in location l. You need to implement this. 
     * Don't forget to flip any necessary pieces (you wrote a function for finding out which ones), as well as placing this piece
     */
    def boardAfterMove(l:Location, p:Player):Board = {
        //I need to find every position that needs to be flipped
        //I then need to update the board for every flip
        //if the move isn't valid return the board as is
        //Should probably throw an error?
        //I'll check if a move is valid before updating the board but I'll put
        //this here just in case one gets through someone
        if (!isValidMove(l, p)) return board

        val flipped = playingHereFlips(l, p)
        
        def updateBoard(p: Player, flipped: Seq[Location], board:Board):Board = {
            // println(s"${board}")
            flipped match
                case Nil => board
                case _ => updateBoard(p, flipped.tail, board.updated(flipped.head, p))
        } 
        updateBoard(p, flipped, board.updated(l, p))
    }

    /** 
     * Written for you, this will print out a Reversi board using ASCII characters. * for black, O for white, . for empty 
     * e.g. println(board.toPrettyString)
     */
    def toPrettyString:String = 
        val sb = StringBuffer()
        (for y <- (0 until boardSize) yield
            (for x <- (0 until boardSize) yield 
                board.get((x, y)) match {
                    case Some(Player.Black) => '*'
                    case Some(Player.White) => 'O'
                    case None => '.'
                }
            ).mkString
        ).mkString("\n")
}

/** A companion object for Boards, just so we can have Board.fromPrettyString */
object Board {
    /** Parses an ASCII representation of a board into a Board, using the same format as board.toPrettyString */
    def fromPrettyString(s:String):Board = 
        val lines = s.linesIterator.toSeq
        val pieces = for 
            x <- 0 until boardSize
            y <- 0 until boardSize 
            p <- lines(y).charAt(x) match {
                case '*' => Some(Player.Black)
                case 'O' => Some(Player.White)
                case _ => None
            }
        yield ((x,y) -> p)
        pieces.toMap
}

/**
  * The state of the board
  * 
  * @param lastMove - the location of the last move
  * @param board - maps the locations of pieces on the board (note that if a piece has not been played in a square, it won't be in the map)
  * @param turn - whose turn it is next. If the game is over, this is None.
  */
case class GameState(lastMove:Option[(Location, Player)], board:Board, turn:Option[Player]) {

    /** True if neither player can play a move. You need to implement this */
    def gameOver:Boolean = {

        turn match
            case Some(player) => {
                if (board.allValidMoves(player).length == 0 && board.allValidMoves(player.opponent).length == 0) true
                else 
                    false
            }
            case None => true
    }
        
    

    /** Whether a particular move is valid. */
    def isValidMove(location:Location):Boolean = 
        turn match {
            case Some(p) => board.isValidMove(location, p)
            case None => false
        }        

    /** Whether a particular move is valid. */
    def allValidMoves(location:Location):Seq[Location] = 
        turn match {
            case Some(p) => board.allValidMoves(p)
            case None => Seq.empty
        }

    /** 
     * Performs a move. You need to implement this. You can use board.boardAfterMove(turn) to get what the board should look like.
     * Don't forget you'll need to work out whether it becomes the other player's turn. 
     * Hint: Assume it does and create the next game state. Then ask the next game state for the number of valid moves. If it's zero, 
     */
    def move(location:Location):GameState = {
        //Is it someone's turn? If so get them to perform a move
        //If not the game has ended either in a win or a stalemate so we'll just return the state as is
        val gameState = turn match
            case Some(player) => new GameState(lastMove = Some((location, player)), board.boardAfterMove(location, player), turn = None)
            case _ => this

        //Checking to see who's move it is
        //The turns may not alternate because after a player p places their piece there may be no valid moves for p.opponent
        //If that's the case we'll set turn to be p because this player will go again
        //If it's not the case we'll set turn to p.opponent
        gameState.lastMove match
            case Some(location, player) => {
                if(gameState.board.allValidMoves(player.opponent).length > 0) gameState.copy(turn = Some(player.opponent))
                else if (gameState.board.allValidMoves(player).length > 0) gameState.copy(turn = Some(player))
                else gameState
            }
            case _ => gameState
    }
}

object GameState {
    def newGame = GameState(None, Map.empty, Some(Player.Black))
}

/** A game is a sequence of game-states (so it remembers past moves). The most recent move is at the end. */
type Game = Seq[GameState]

/** Creates a new game, containing just the start game state */
def newGame:Seq[GameState] = Seq(GameState.newGame)

/** Called by the UI to make your AI play the game. You need to implement this. */
def chooseMove(state:Seq[GameState]):Location = {

    val player = state.last.turn.get
    /* 
    We need to pick a move, if I had more time I might implement mini-max or something,a rudimentary "real" AI
    to play the game, but it feels beyond the scope and I don't really have the time, I'll just go for a greedy algorithm
    that picks whatever move flips the most pieces, it's locally optimal but globally probably not so much.
    */
    val possibleMoves = state.last.board.allValidMoves(player)
    possibleMoves.maxBy(location => state.last.board.playingHereFlips(location, player).length)
}