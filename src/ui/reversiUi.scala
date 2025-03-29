package assignmentTwo.ui

import com.wbillingsley.veautiful.*
import com.wbillingsley.veautiful.html.*  
import com.wbillingsley.veautiful.doctacular.* 

import assignmentTwo.reversi.* 



val ReversiStyle = Styling(
    """|
       |""".stripMargin
).modifiedBy(
     " .row" -> "display: flex;",
     " .cell" -> "display: inline-block; width: 30px; height: 30px; background: #5abd72; border: 1px solid #dddddd;",
     " .piece.black" -> "fill: black;",
     " .piece.white" -> "fill: white;",
     " .moveWidget" -> "fill: none; stroke: #0698b5; stroke-width: 4;",
     " .moveButton.black" -> "background: #444; color: white;",
     " .moveButton.white" -> "background: #eee; color: black;"
).register()


class ReversiWidget(initial:Game) extends DHtmlComponent with Keep("") {

    val state = stateVariable(initial)
    val human = stateVariable(Player.Black)

    // Whether it's the human's turn
    def humanMove:Boolean = state.value.last.turn.contains(human.value)

    // Whether it's the ai's turn
    def aiMove:Boolean = state.value.last.turn.contains(human.value.opponent)

    // The graphic for a piece on the board
    def piece(colour:String) = SVG.svg(^.attr.width := "30", ^.attr.height:= "30",
        SVG.circle(^.attr.cx := 15, ^.attr.cy := 15, ^.attr.r := 10, ^.cls := "piece " + colour)
    )

    // The widget to click to play a human move
    def moveWidget(x:Int, y:Int) = SVG.svg(^.attr.width := "30", ^.attr.height:= "30",
        SVG.circle(^.attr.cx := 15, ^.attr.cy := 15, ^.attr.r := 10, ^.cls := "moveWidget "),
        ^.onClick --> { state.value = (state.value :+ state.value.last.move((x, y))) }
    )

    // Renders the state of the game
    def render =  try {
        <.div(^.cls := ReversiStyle,

            <.div(
                <.label("Human plays as "), 
                <.input(^.attr.`type` := "radio", (^.prop.checked := human.value == Player.Black), ^.onInput --> { human.value = Player.Black }), <.label(" Black "),
                <.input(^.attr.`type` := "radio", (^.prop.checked := human.value == Player.White), ^.onInput --> { human.value = Player.White }), <.label(" White "),
            ),

            (for row <- 0 until boardSize yield 
                <.div(^.cls := "row",
                    for column <- 0 until boardSize yield 
                        <.div(^.cls := "cell", 
                            state.value.last.board.get((column, row)) match {
                                case Some(Player.Black) => piece("black")
                                case Some(Player.White) => piece("white")
                                case None => 
                                    if humanMove then
                                        try {
                                            if state.value.last.isValidMove((column, row)) then moveWidget(column, row) else None
                                        } catch {
                                            case x:Throwable => "???"
                                        }                                        
                                    else 
                                        None
                            }
                            
                        
                        )
                )
            ),

            <.button("Play AI move", ^.prop.disabled := !aiMove, ^.onClick --> {  state.value = (state.value :+ state.value.last.move(chooseMove(state.value))) }),

            <.p(
                <.label(" Black: "), "" + (try { state.value.last.board.piecesFor(Player.Black) } catch { case x:Throwable => x.getMessage }),
                <.label(" White: "), "" + (try { state.value.last.board.piecesFor(Player.White) } catch { case x:Throwable => x.getMessage }),

            ),

            <.div(
                <.p(
                    "The widget below shows the history of the game. Click on a move to backtrack to the board just after that move"
                ),

                <.table(
                    <.tr(<.th(), <.th("Black"), <.th("White")), 
                    {
                        for (pairs, i) <- state.value.tail.grouped(2).toSeq.zipWithIndex yield <.tr(
                            <.td("" + (i + 1)),
                            for 
                                s <- pairs
                                (loc, p) <- s.lastMove
                            yield <.td(
                                <.button(^.cls := (if p == Player.Black then "moveButton black" else "moveButton white"), loc.toChessLocation, ^.onClick --> { state.value = state.value.takeWhile(_ != s) :+ s })
                            )
                        )
                    }
                )
            )

        )
    } catch {
        case sx:StudentException => 
            <.div(
                <.h2(s"Your code threw an exception when I tried to call ${sx.callName}"),
                <.pre(sx.x.toString()),
                <.pre(sx.getStackTrace().mkString("\n")),
            )
        case (x:Throwable) => 
            // x.printStackTrace()
            <.div(
                <.h2("An exception occured"),
                <.pre(x.toString()),
                // <.pre(x.getStackTrace().mkString("\n")),
            )
    }

}


def reversiPage = <.div(
    <.h2("Reversi"),
    <.p("The widget below will let you play against your AI."),
    <.p(" When it's the human's turn, valid moves will appear with circles to click on. When it's the AI's move, the 'play AI move' button will be enabled."),
    <.p(" You can switch whether you are playing as black or white at any time"),
    ReversiWidget(newGame)
)