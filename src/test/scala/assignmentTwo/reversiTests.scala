//> using test.dep "org.scalameta::munit::1.1.0"

package assignmentTwo.reversi

class ReversiTests extends munit.FunSuite {

    // Test created by Will used in marking
    test("Walking to the edge of the board ") {
        assertEquals(walkToEdge((3, 2), Direction.North), Seq((3, 2), (3, 1), (3, 0)))
        assertEquals(walkToEdge((3, 2), Direction.South), Seq((3, 2), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7)))
        assertEquals(walkToEdge((3, 0), Direction.North), Seq((3, 0)))
        assertEquals(walkToEdge((3, 7), Direction.South), Seq((3, 7)))
        assertEquals(walkToEdge((3, -1), Direction.North), Seq.empty)
        assertEquals(walkToEdge((3, 8), Direction.South), Seq.empty)

        assertEquals(walkToEdge((3, 2), Direction.East), Seq((3, 2), (4, 2), (5, 2), (6, 2), (7, 2)))
        assertEquals(walkToEdge((3, 2), Direction.West), Seq((3, 2), (2, 2), (1, 2), (0, 2)))
        assertEquals(walkToEdge((0, 3), Direction.West), Seq((0, 3)))
        assertEquals(walkToEdge((7, 3), Direction.East), Seq((7, 3)))
        assertEquals(walkToEdge((-1, 3), Direction.East), Seq.empty)
        assertEquals(walkToEdge((8, 3), Direction.West), Seq.empty)

        assertEquals(walkToEdge((3, 2), Direction.NorthEast), Seq((3, 2), (4, 1), (5, 0)))
        assertEquals(walkToEdge((3, 2), Direction.SouthWest), Seq((3, 2), (2, 3), (1, 4), (0, 5)))
        assertEquals(walkToEdge((0, 3), Direction.SouthWest), Seq((0, 3)))
        assertEquals(walkToEdge((7, 3), Direction.NorthEast), Seq((7, 3)))
        assertEquals(walkToEdge((-1, 3), Direction.NorthEast), Seq.empty)
        assertEquals(walkToEdge((8, 3), Direction.SouthWest), Seq.empty)

        assertEquals(walkToEdge((3, 2), Direction.SouthEast), Seq((3, 2), (4, 3), (5, 4), (6, 5), (7, 6)))
        assertEquals(walkToEdge((3, 2), Direction.NorthWest), Seq((3, 2), (2, 1), (1, 0)))
        assertEquals(walkToEdge((3, 0), Direction.NorthWest), Seq((3, 0)))
        assertEquals(walkToEdge((3, 7), Direction.SouthEast), Seq((3, 7)))
        assertEquals(walkToEdge((3, -1), Direction.SouthEast), Seq.empty)
        assertEquals(walkToEdge((3, 8), Direction.NorthWest), Seq.empty)
    }


    // Test created by Will used in marking
    test("the count of pieces of each colour is returned correctly") {
        assertEquals(3, 
            Board.fromPrettyString("""|........
                                      |........
                                      |........
                                      |........
                                      |...OOO..
                                      |........
                                      |........
                                      |........""".stripMargin).pieces
        )

        assertEquals(3, 
            Board.fromPrettyString("""|........
                                      |........
                                      |........
                                      |........
                                      |...OOO..
                                      |........
                                      |........
                                      |........""".stripMargin).piecesFor(Player.White)
        )

        assertEquals(4, 
            Board.fromPrettyString("""|........
                                      |........
                                      |........
                                      |...*....
                                      |...OOO..
                                      |...***..
                                      |........
                                      |........""".stripMargin).piecesFor(Player.Black)
        )
    }    

    // Test created by Will used in marking
    test("isValidMove returns false if there's no opposing pieces flipped") {
        assertEquals(false, Board.fromPrettyString("""|........
                                                      |........
                                                      |........
                                                      |........
                                                      |...OOOO.
                                                      |........
                                                      |........
                                                      |........""".stripMargin).isValidMove((2, 4), Player.White))

        assertEquals(false, Board.fromPrettyString("""|........
                                                      |........
                                                      |........
                                                      |........
                                                      |...****.
                                                      |........
                                                      |........
                                                      |........""".stripMargin).isValidMove((2, 4), Player.White))                                  

        assertEquals(false, Board.fromPrettyString("""|........
                                                      |........
                                                      |........
                                                      |........
                                                      |...OO**.
                                                      |........
                                                      |........
                                                      |........""".stripMargin).isValidMove((2, 4), Player.White))                                                                                        

    }

    // Test created by Will used in marking
    test("isValidMove returns true if there are pieces flipped") {
        assertEquals(true, Board.fromPrettyString("""|........
                                                     |........
                                                     |........
                                                     |........
                                                     |...*OOO.
                                                     |........
                                                     |........
                                                     |........""".stripMargin).isValidMove((2, 4), Player.White))

        assertEquals(true, Board.fromPrettyString("""|........
                                                     |.....*..
                                                     |....O...
                                                     |...*....
                                                     |...***..
                                                     |........
                                                     |........
                                                     |........""".stripMargin).isValidMove((2, 4), Player.White))                                  

        assertEquals(true, Board.fromPrettyString("""|........
                                                     |........
                                                     |........
                                                     |........
                                                     |...*OOO.
                                                     |........
                                                     |........
                                                     |........""".stripMargin).isValidMove((2, 4), Player.White))                                                                                        

    }

    // Test created by Will used in marking
    test("at the start of the game, only central moves are valid") {

        assertEquals(true, Board.fromPrettyString("""|........
                                                     |........
                                                     |........
                                                     |...*....
                                                     |........
                                                     |........
                                                     |........
                                                     |........""".stripMargin).isValidMove((3, 4), Player.White)) 

        assertEquals(false, Board.fromPrettyString("""|........
                                                      |........
                                                      |........
                                                      |...*....
                                                      |........
                                                      |........
                                                      |........
                                                      |........""".stripMargin).isValidMove((3, 3), Player.White)) 

        assertEquals(false, Board.fromPrettyString("""|........
                                                      |........
                                                      |........
                                                      |...*....
                                                      |........
                                                      |........
                                                      |........
                                                      |........""".stripMargin).isValidMove((3, 5), Player.White)) 

    }

    test("playingHereFlips returns sequence of flipped pieces if they exist") {

        assertEquals(Seq((3,4)), Board.fromPrettyString("""|........
                                                     |........
                                                     |........
                                                     |........
                                                     |...*OOO.
                                                     |........
                                                     |........
                                                     |........""".stripMargin).playingHereFlips((2, 4), Player.White))
    // Feel free to add more tests of your own here
    
    }
    test("boardAfterMove updates the board ") {
        val boardA = Board.fromPrettyString("""|........
                                               |........
                                               |........
                                               |........
                                               |..OOOOO.
                                               |........
                                               |........
                                               |........""".stripMargin)
        val boardB = Board.fromPrettyString("""|........
                                               |........
                                               |........
                                               |........
                                               |...*OOO.
                                               |........
                                               |........
                                               |........""".stripMargin)

        val boardC = Board.fromPrettyString("""|........
                                               |........
                                               |........
                                               |...O*...
                                               |...*O...
                                               |........
                                               |........
                                               |........""".stripMargin)

        val boardD = Board.fromPrettyString("""|........
                                               |........
                                               |........
                                               |...O*...
                                               |...**...
                                               |....*...
                                               |........
                                               |........""".stripMargin) // (4 ,5)\

        val boardE = Board.fromPrettyString("""|........
                                               |........
                                               |........
                                               |...O*...
                                               |...*O...
                                               |....*O..
                                               |........
                                               |........""".stripMargin) //(5,5)

        assertEquals(boardC.boardAfterMove((4,5), Player.Black), boardD)

        assertEquals(boardD.boardAfterMove((5,5), Player.White), boardE)

        assertEquals(boardB.boardAfterMove((2,4), Player.White), boardA)
        // assertEquals(Board.fromPrettyString("""|........
        //                                        |........
        //                                        |........
        //                                        |........
        //                                        |..OOOOO.
        //                                        |........
        //                                        |........
        //                                        |........""".stripMargin), Board.fromPrettyString("""|........
        //                                              |........
        //                                              |........
        //                                              |........
        //                                              |...*OOO.
        //                                              |........
        //                                              |........
        //                                              |........""".stripMargin).boardAfterMove((2, 4), Player.White))
    // Feel free to add more tests of your own here
    
    }
    test("move correctly updates the gamestate"){

        
        val boardA = Board.fromPrettyString("""|........
        |........
        |........
        |........
        |........
        |........
        |........
        |........""".stripMargin)

        val boardB = Board.fromPrettyString("""|........
                                                |........
                                                |........
                                                |...*O...
                                                |....*...
                                                |........
                                                |........
                                                |........""".stripMargin) //(4,4)

        val boardC = Board.fromPrettyString("""|........
                                                |........
                                                |........
                                                |...*O...
                                                |...O*...
                                                |........
                                                |........
                                                |........""".stripMargin) //(3,4)
                                        
        
        val gameState1 = GameState(lastMove = Some((4,4), Player.Black), board = boardB, turn = Some(Player.White))
        val gameState2 = GameState(lastMove = Some((3,4), Player.White), board = boardC, turn = Some(Player.Black))


        assertEquals(gameState1.move(3,4), gameState2)        
    }

    test("chooseMove correctly chooses a move"){
        val boardB = Board.fromPrettyString("""|........
                                                |........
                                                |........
                                                |...*O...
                                                |....*...
                                                |........
                                                |........
                                                |........""".stripMargin) //(4,4)


        val boardC = Board.fromPrettyString("""|........
                                               |........
                                               |........
                                               |...*....
                                               |........
                                               |........
                                               |........
                                               |........""".stripMargin)

        val gameState1 = GameState(lastMove = Some((4,4), Player.Black), board = boardB, turn = Some(Player.White))
        val gameState2 = GameState(lastMove = Some((3,3), Player.Black), board = boardC, turn = Some(Player.White))
        val gameSeq: Seq[GameState] = Seq(gameState1)
        val gameSeq2: Seq[GameState] = Seq(gameState2)

        assertEquals((3,4) ,chooseMove(gameSeq))
        // assertEquals((4,3), chooseMove(gameSeq2))
    }
}