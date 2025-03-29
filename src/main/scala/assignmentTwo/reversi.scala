package assignmentTwo.reversi

import scala.collection.immutable.Queue

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
def walkToEdge(start:Location, d:Direction):Seq[Location] = 
    ???

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
        ???
        

    /** The number of pieces the given player has. You need to implement this */
    def piecesFor(p:Player):Int = 
        ???

    /**
      * If player p placed a piece in location l, what are the locations it would flip? You need to implement this.
      * 
      * Hint: For each direction, get the locations that walk in that direction while it contains pieces (starting one step in that direction)
      *       if there aren't any of your pieces, you can't capture anything (you've got nothing on the other side)
      *       if there is one of your pieces on that line, take all the pieces up to your first piece on the line
      */
    def playingHereFlips(l:Location, p:Player):Seq[Location] = 
        ???
        


    // Whether it'd be valid for player p to play in location l (assuming it's their turn). You need to implement this.
    // Hint: If there are fewer than 4 pieces on the board, it is a valid move if it is in one of the centre squares that is not already occupied
    // Hint: Otherwise, it's a valid move if it is empty and flips more than 0 pieces.
    def isValidMove(l:Location, p:Player):Boolean = 
        ???

    // All the moves that are valid from this position for player p. You need to implement this.
    // Hint: Get a list of every location from (0, 0) to (7, 7) and filter it for the ones that are valid
    def allValidMoves(p:Player):Seq[Location] = 
        ???

    /** 
     * Returns a board where player p has placed a piece in location l. You need to implement this. 
     * Don't forget to flip any necessary pieces (you wrote a function for finding out which ones), as well as placing this piece
     */
    def boardAfterMove(l:Location, p:Player):Board = 
        ??? 

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
    def gameOver:Boolean = ???

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
    def move(location:Location):GameState = 
        ???
        
        

}

object GameState {
    def newGame = GameState(None, Map.empty, Some(Player.Black))
}

/** A game is a sequence of game-states (so it remembers past moves). The most recent move is at the end. */
type Game = Seq[GameState]

/** Creates a new game, containing just the start game state */
def newGame:Seq[GameState] = Seq(GameState.newGame)

/** Called by the UI to make your AI play the game. You need to implement this. */
def chooseMove(state:Seq[GameState]):Location = 
    ???
    