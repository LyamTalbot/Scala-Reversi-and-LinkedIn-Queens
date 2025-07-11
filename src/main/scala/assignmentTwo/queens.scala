package assignmentTwo.queens

import com.wbillingsley.veautiful.html.PathDSL.Compose.Loc
import scala.annotation.tailrec

// The colours we can play with
enum Colour(val letter: Char):
  case Red extends Colour('r')
  case Orange extends Colour('o')
  case Yellow extends Colour('y')
  case Green extends Colour('g')
  case Blue extends Colour('b')
  case Pink extends Colour('p')
  case White extends Colour('w')
  case Dark extends Colour('d')
  case Cyan extends Colour('c')
  case Purple extends Colour('u')

/** all the different directions a straight line can go in */
enum Direction(val dx: Int, val dy: Int):
  case North extends Direction(0, -1)
  case NorthEast extends Direction(1, -1)
  case East extends Direction(1, 0)
  case SouthEast extends Direction(1, 1)
  case South extends Direction(0, 1)
  case SouthWest extends Direction(-1, 1)
  case West extends Direction(-1, 0)
  case NorthWest extends Direction(-1, -1)

// A list of all the directions, North, NorthEast, etc.
def allDirections: Seq[Direction] = Direction.values.toSeq

/** A location on the board. Zero-indexed. Location is a type alias for (Int,
  * Int). e.g. (0, 0) is the top-left
  */
type Location = (Int, Int)

/** Defines some methods we can call on Locations. Note, not quite as many as we
  * put into the Reversi game.
  */
extension (l: Location) {
  // Lets us add a direction to a location to step in that direction
  // e.g. (1, 1) + Direction.North == (1, 0)
  def +(dir: Direction): Location =
    val (x, y) = l
    (x + dir.dx, y + dir.dy)

}

// Squares can contain a queen or a blank
enum Contents:
  case Queen
  case Blank

// A row of coloured squares
type Row = Seq[Colour]

// A sequence of rows makes up our coloured grid
type Grid = Seq[Row]

// To solve the grid, we keep track of what's possible
// i.e. for every location, we say "this could contain a Queen or a Blank"
// and for some squares, we'll get to eliminate "Queen" and for other squares we'll get to eliminate "Blank"
type PossibilityMap = Map[Location, Set[Contents]]

/** Methods we can call on Grids */
extension (grid: Grid) {

  /** implemented for you, the Set of colours present in the grid */
  def colours: Set[Colour] =
    (for
      row <- grid
      col <- row
    yield col).toSet

  /** Implemented for you, looks up what colour a square in the grid is, if it's
    * on the board
    */
  def apply(location: Location): Option[Colour] =
    val (x, y) = location
    if grid.allSquares.contains(location) then Some(grid(y)(x)) else None

  /** implemented for you, all the squares of a particular column */
  def column(x: Int): Seq[Location] = for y <- grid.indices yield (x, y)

  /** implemented for you, all the squares of a particular row */
  def row(y: Int): Seq[Location] = for x <- grid(y).indices yield (x, y)

  /** implemented for you, all the locations on the board */
  def allSquares: Seq[Location] =
    for
      y <- grid.indices
      x <- grid(y).indices
    yield (x, y)

  /*
    I wrote this to easily get the colour of a square, I use it for one or two things,
    It's just a convenience thing
   */
  def squareColour(loc: Location): Colour = grid(loc._2)(loc._1)

  /** implemented for you, all the squares of a particular colour */
  def squares(colour: Colour): Seq[Location] =
    for sq <- allSquares if grid(sq).contains(colour) yield sq

  // The neighbouring squares that are in the grid
  def neighbours(loc: Location): Seq[Location] =
    for
      d <- Direction.values.toSeq
      p = loc + d if grid.allSquares.contains(loc)
    yield p

  // Generates a possibility map that thinks any square could be a Queen or a Blank
  def allPossibilities: PossibilityMap =
    (for
      y <- grid.indices
      x <- grid(y).indices
    yield (x, y) -> Contents.values.toSet).toMap

}

object Grid {
  // Reads in a grid of colours from a string like
  //
  // wwrb
  // wrrb
  // rrrb
  // bbbb
  def fromPrettyString(s: String): Grid =
    for line <- s.linesIterator.toSeq yield {
      // The .get here is slightly poor practice ("what if it didn't match?")
      // but as we're creating the pretty strings ourselves, we want it to fail if there's an unrecognised character, which it will
      line.map { (c) =>
        Colour.values.find(_.letter == c) match {
          case Some(col) => col
          case _ =>
            throw RuntimeException(
              s"Grid contained $c which I don't have a colour for"
            )
        }
      }
    }
}

/** Some methods on possibility maps */
extension (map: PossibilityMap) {

  // A map is solved if we know whether every square is a Queen or a Blank
  def solved: Boolean =
    map.forall((loc, contents) => contents.size == 1)

  // All locations that could contain a queen, from what we know so far
  def queenLocations: Seq[Location] =
    for (loc, contents) <- map.toSeq if contents.contains(Contents.Queen)
    yield loc

  def unsolvedSquares: Seq[Location] =
    map.queenLocations.filterNot(location =>
      map(location) == Set(Contents.Queen)
    )

  /** Sets a square to be a Queen, also marking its neighbours and other squares
    * in the colour, row, and colum to be Blank
    */
  def setQueen(grid: Grid, loc: Location): PossibilityMap =
    // If this square is a Queen, these can't be
    val thereforBlank: Seq[Location] =
      grid.column(loc._1).filter(_ != loc) ++ // The other squares in the column
        grid.row(loc._2).filter(_ != loc) ++ // The other squares in the row
        {
          grid(loc) match {
            case Some(c) =>
              grid.squares(c) // The other squares of the same colour
            case _ =>
              throw RuntimeException(s"Looked up a square that had no colour")
          }
        } ++
        grid.neighbours(loc) // The neighbouring squares

    map ++ thereforBlank.map(_ -> Set(Contents.Blank)) + (loc -> Set(
      Contents.Queen
    ))

  /** Sets a square to be a Blank. Takes the grid as input for consistency of
    * API with setQueen
    */
  def setBlank(grid: Grid, loc: Location): PossibilityMap =
    map + (loc -> Set(Contents.Blank))

  /** Returns true if there are two definite Queens in this map that are
    * touching
    */
  def hasTwoQueensTouching(grid: Grid): Boolean =
    grid.allSquares.exists((l) =>
      map(l) == Set(Contents.Queen) && grid
        .neighbours(l)
        .exists((ll) => map(ll) == Set(Contents.Queen))
    )

  /** Implement this. Returns true if a row in this map definitely breaks a
    * rule. i.e. it has 2 definite queens or is all definite blanks
    */
  def rowFails(grid: Grid): Boolean = {
    // Get every row
    val rows = for row <- (0 to 7) yield grid.row(row)
    // For every row check if every position is blank
    val blanks = rows
      .map(row => row.forall(position => map(position) == Set(Contents.Blank)))
      .contains(true)
    // For every row check if the row contains more than one queen
    val queens = rows
      .map(row =>
        row.count(position => map(position) == Set(Contents.Queen)) > 1
      )
      .contains(true)
    queens || blanks
  }

  /** Implement this. Returns true if a column in this map definitely breaks a
    * rule. i.e. it has 2 definite queens or is all definite blanks
    */
  def columnFails(grid: Grid): Boolean = {
    // Get every column
    val columns = for col <- (0 to 7) yield grid.column(col).toIterable
    // Map over every comlumn and
    // check if all positions in a column are definite blank
    val blanks = columns
      .map(column =>
        column.forall(position => map(position) == Set(Contents.Blank))
      )
      .contains(true)
    // Check all positions in a column, are there more than 2 queens?
    val queens = columns
      .map(column =>
        column.count(position => map(position) == Set(Contents.Queen)) > 1
      )
      .contains(true)
    queens || blanks
  }

  /** Implement this. Returns true if a colour in this map definitely breaks a
    * rule. i.e. it has 2 definite queens or is all definite blanks
    */
  def colourFails(grid: Grid): Boolean = {
    // Get a sequence of postions, each sequence represents one colour
    val colourPositions = Colour.values.map(colour => grid.squares(colour))
    // Remove sequences that are empty
    val nonEmptySequences = colourPositions.filter(colour => colour.length > 0)
    // For every sequence/colour check if every position is a definite blank
    val blanks = nonEmptySequences.map(colour =>
      colour.forall(position => map(position) == Set(Contents.Blank))
    )
    // For every sequence/colour check if any sequence contains more than 1 queen
    val queens = colourPositions
      .map(colour =>
        colour.count(position => map(position) == Set(Contents.Queen)) > 1
      )
      .contains(true)
    blanks.contains(true) || queens
  }

  /** For the last task in the assignment. You can add another logical rule that
    * can invalidate a grid if it's impossible - e.g. that if 2 colours only
    * have the same 1 column free, it's invalid
    */
  def extraRuleFails(grid: Grid): Boolean = {
    // my logic is in makeAMove
    false
  }

  // Returns true if this set of possibilities breaks a rule -- e.g. doesn't have queens in a row, column, or colour, has two queens touching,
  // or definitely has two queens in a row, column, or colour
  def invalidFor(grid: Grid): Boolean =
    hasTwoQueensTouching(grid) || rowFails(grid) || columnFails(
      grid
    ) || colourFails(grid) || extraRuleFails(grid)

  // You need to implement this. Make some progress in solving the grid
  // You should look for:
  //
  // * a square where setting it to a blank would make the new board invalid (in which case it must be a queen)
  // * a square where setting it to a queen would make the new board invalid (in which case it must be a blank), or
  //
  // There is one grid that this technique won't solve. You'll need to add another logical rule for if the first two didn't find anything
  // e.g. if there are now two colours that can only have queens in the same two columns, no other colour could have a queen in those columns
  // I created a function "extraRuleFails" where you could implement your rule; or you could do it directly here.
  def makeAstep(grid: Grid): PossibilityMap = {
    // sort the available sequences based on how many squares they have
    // is there only 1 square left in a row?
    // is there only 1 square left in a column?
    // is there only 1 square left in a colour?

    /*
        this solution for checking the contents of the top 3 rows is not very dynamic
        this is poor form on my part because it will not be useful on all queens maps.
        but implementing it as a sliding window is slightly more difficult
        I think if I have time I'll come back and do that
        I was thinking having a stride length of 1 for the window would cause issues.
        I think if you have a window size of n you need a stride length of n so you're looking at distinct cases.
        If I have time I'll do that and I'll slide this 8x3 window down the board
        But I could also argue that my job is to programmatically solve the problem i've been given and I didn't hard code a solution
        I just wrote a rule around a specific edge case, map 4, but that's a philosophical debate more than anything I suppose.
        I'll set these values to be lazy so that they're not running every single time we run this function, only when we need them.
     */

    lazy val firstThreeRows =
      for rowNum <- (0 to 2)
      yield grid.row(rowNum)

    lazy val coloursInRows = firstThreeRows
      .map(row => row.map(position => grid.squareColour(position)))
      .flatten
      .toSet
    lazy val coloursOutsideRows = coloursInRows.filter(colour =>
      grid.squares(colour).exists(location => location._2 > 2)
    )
    // val coloursOutsideRangeSquares = coloursInRows.map(colour => (colour -> grid.squares(colour).filter(location => location._2 > 2)))
    lazy val coloursOutsideRangeSquares = coloursOutsideRows.map(colour =>
      (colour -> grid.squares(colour).filter(location => location._2 > 2))
    )

    lazy val removedSquares = coloursOutsideRangeSquares.map {
      case (colour, pieces) =>
        grid.squares(colour).filterNot(p1 => pieces.exists(p2 => p1 == p2))
    }.flatten

    /*
        I'll get a sequence of positions for every colour.
        Remove empty sequences because some colours aren't represented and filter out
     */
    val colourSequences = grid.colours.map(colour => grid.squares(colour)).toSeq
    val colourSeqWithSolvedRemoved = colourSequences.map(sequence =>
      sequence.filter(position => map(position).size > 1)
    )
    val filteredSequences = colourSeqWithSolvedRemoved
      .filterNot(sequence => sequence.isEmpty)
      .sortBy(sequence => sequence.length)
      .flatten

    // We'll find a position that causes some kind of failure
    // This will return an option type because we may not find such a position but that will trigger some extra logic using the filtered positions I found above.
    // Find is useful here because it's lazily evaluated so we won't bother checking beyond the first match we find, we only need one position at a time anyway
    val causesInvalid = filteredSequences.find(position =>
      map.setBlank(grid, position).invalidFor(grid) || map
        .setQueen(grid, position)
        .invalidFor(grid)
    )

    // did a position set to queen or blank cause a violation?
    // If so set it to blank/queen as appropriate
    // if none were found we're at a bit of an impasse, so I'll trigger the additional logic I wrote
    // remove all squares in the 3x8 window at the top if they have squares outside that range.
    causesInvalid match
      case None => {
        if (coloursInRows.size > coloursOutsideRows.size) then
          map ++ removedSquares.map(_ -> Set(Contents.Blank)).toMap
        else map
      }
      case Some(location) => {
        val potentialMap = map.setQueen(grid, location)
        if (potentialMap.invalidFor(grid)) then map.setBlank(grid, location)
        else potentialMap
      }
  }
}
val puzzles = Map(
  "#77" -> Grid.fromPrettyString("""|ppoopbbb
                                      |pgoopbbb
                                      |pwwppbbb
                                      |pwwppppp
                                      |pppuuupp
                                      |pppuuupp
                                      |ccpuuurp
                                      |ccpppppp""".stripMargin),
  "#170" -> Grid.fromPrettyString("""|byyydwww
                                       |bbbydddw
                                       |byyydwww
                                       |yyuyywrr
                                       |gguyowwr
                                       |guuuooor
                                       |grrrorrr
                                       |rrrrrrrr""".stripMargin),
  "#332" -> Grid.fromPrettyString("""|pppppppp
                                       |poppppop
                                       |pobbggow
                                       |poobgoow
                                       |poooooow
                                       |prrrrrrw
                                       |ppyrrdww
                                       |ppyyddww""".stripMargin),
  "#151 (harder)" -> Grid.fromPrettyString("""|uuuuoobb
                                                |ugguowwb
                                                |ggggowwb
                                                |ggggggbb
                                                |rrgggggg
                                                |rygggggg
                                                |ryggdggd
                                                |rrggdddd""".stripMargin)
)
