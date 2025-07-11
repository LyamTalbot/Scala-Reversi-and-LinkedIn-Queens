//> using test.dep "org.scalameta::munit::1.1.0"

package assignmentTwo.queens

// import scala.collection.immutable.{Map, Set}


/* 
I wrote all off the following unit tests.
 */

class QueensTests extends munit.FunSuite {

  // A place for you to keep tests you write
  test("Row fails if all definite blanks or more than one queen") {
    val testGrid = Grid.fromPrettyString("""|ppoopbbb
                                            |pgoopbbb
                                            |pwwppbbb
                                            |pwwppppp
                                            |pppuuupp
                                            |pppuuupp
                                            |ccpuuurp
                                            |ccpppppp""".stripMargin)
    //Create a base blank map
    val possibilities = testGrid.allPossibilities
    
    //Create specific sequences that will fail

    //If an entire row contains all definite blanks it fails
    val map1:PossibilityMap = Map(
      (0, 0) -> Set(Contents.Blank),
      (0, 1) -> Set(Contents.Blank),
      (0, 2) -> Set(Contents.Blank),
      (0, 3) -> Set(Contents.Blank),
      (0, 4) -> Set(Contents.Blank),
      (0, 5) -> Set(Contents.Blank),
      (0, 6) -> Set(Contents.Blank),
      (0, 7) -> Set(Contents.Blank)
    )

    //If a row contains more than 1 definite queen it fails
    val map2:PossibilityMap = Map(
        (0, 0) -> Set(Contents.Queen),
        (0, 1) -> Set(Contents.Blank),
        (0, 2) -> Set(Contents.Blank),
        (0, 3) -> Set(Contents.Blank),
        (0, 4) -> Set(Contents.Blank),
        (0, 5) -> Set(Contents.Queen),
        (0, 6) -> Set(Contents.Blank),
        (0, 7) -> Set(Contents.Blank)
        )
    
    //This row is correct
    val map3:PossibilityMap = Map(
        (0, 0) -> Set(Contents.Blank, Contents.Queen),
        (0, 1) -> Set(Contents.Blank),
        (0, 2) -> Set(Contents.Blank),
        (0, 3) -> Set(Contents.Blank),
        (0, 4) -> Set(Contents.Blank),
        (0, 5) -> Set(Contents.Blank),
        (0, 6) -> Set(Contents.Blank),
        (0, 7) -> Set(Contents.Blank)
        )

    //Add the test rows to the base map, only updating the locations that have new values            
    val testMap1 = possibilities ++ map1
    val testMap2 = possibilities ++ map2
    val testMap3 = possibilities ++ map3

    //Run tests.
    assertEquals(testMap1.columnFails(testGrid), true)
    assertEquals(testMap2.columnFails(testGrid), true)
    assertEquals(testMap3.columnFails(testGrid), false)
  }

    test("Column fails if all definite blanks or more than 1 definite queen") {
    //Same as above
    val testGrid = Grid.fromPrettyString("""|ppoopbbb
                                            |pgoopbbb
                                            |pwwppbbb
                                            |pwwppppp
                                            |pppuuupp
                                            |pppuuupp
                                            |ccpuuurp
                                            |ccpppppp""".stripMargin)
    
    val possibilities = testGrid.allPossibilities
    val map1:PossibilityMap = Map(
      (0, 0) -> Set(Contents.Blank),
      (1, 0) -> Set(Contents.Blank),
      (2, 0) -> Set(Contents.Blank),
      (3, 0) -> Set(Contents.Blank),
      (4, 0) -> Set(Contents.Blank),
      (5, 0) -> Set(Contents.Blank),
      (6, 0) -> Set(Contents.Blank),
      (7, 0) -> Set(Contents.Blank)
    )

    
    val map2:PossibilityMap = Map(
        (0,0) -> Set(Contents.Queen),
        (1,0) -> Set(Contents.Blank),
        (2,0) -> Set(Contents.Blank),
        (3,0) -> Set(Contents.Blank),
        (4,0) -> Set(Contents.Blank),
        (5,0) -> Set(Contents.Queen),
        (6,0) -> Set(Contents.Blank),
        (7,0) -> Set(Contents.Blank)
        )
        
        val map3:PossibilityMap = Map(
            (0,0) -> Set(Contents.Blank, Contents.Queen),
            (1,0) -> Set(Contents.Blank),
            (2,0) -> Set(Contents.Blank),
            (3,0) -> Set(Contents.Blank),
            (4,0) -> Set(Contents.Blank),
            (5,0) -> Set(Contents.Blank),
            (6,0) -> Set(Contents.Blank),
            (7,0) -> Set(Contents.Blank)
            )
            
    val testMap1 = possibilities ++ map1
    val testMap2 = possibilities ++ map2
    val testMap3 = possibilities ++ map3

    assertEquals(testMap1.rowFails(testGrid), true)
    assertEquals(testMap2.rowFails(testGrid), true)
    assertEquals(testMap3.rowFails(testGrid), false)
  }

  test("Colour fails if all colour squares are definite blanks or more than one definite queen"){
    //Same as above but per colour not per row/column
    val testGrid = Grid.fromPrettyString("""|ppoopbbb
                                            |pgoopbbb
                                            |pwwppbbb
                                            |pwwppppp
                                            |pppuuupp
                                            |pppuuupp
                                            |ccpuuurp
                                            |ccpppppp""".stripMargin)
    val possibilities = testGrid.allPossibilities

    val map1:PossibilityMap = Map((2,0) -> Set(Contents.Blank),
                                (3,0) -> Set(Contents.Blank),
                                (2,1) -> Set(Contents.Blank),
                                (3,1) -> Set(Contents.Blank))

    val map2:PossibilityMap = Map((2,0) -> Set(Contents.Queen),
                                (3,0) -> Set(Contents.Queen),
                                (2,1) -> Set(Contents.Blank),
                                (3,1) -> Set(Contents.Blank))

    val map3:PossibilityMap = Map((2,0) -> Set(Contents.Queen),
                                (3,0) -> Set(Contents.Blank),
                                (2,1) -> Set(Contents.Blank),
                                (3,1) -> Set(Contents.Blank))


    val testMap1 = testGrid.allPossibilities ++ map1
    val testMap2 = testGrid.allPossibilities ++ map2
    val testMap3 = testGrid.allPossibilities ++ map3
    // testGrid.allPossibilities.foreach{
    //     case (position, value) => println(s"$position -> $value")
    // }

    assertEquals(testGrid.allPossibilities.colourFails(testGrid), false)
    assertEquals(testMap1.colourFails(testGrid), true)
    assertEquals(testMap2.colourFails(testGrid), true)
    assertEquals(testMap3.colourFails(testGrid), false)

}

}
