package The_Last_Crusade_Episode_1

/**
  * Created by Kotobotov.ru on 02.06.2018.
  * https://www.codingame.com/ide/puzzle/the-last-crusade-episode-1
  *
  */
import math._
import scala.util._

object Player extends App {
  // w: number of columns.
  // h: number of rows.
  case class Pos(x: Int, y: Int)

  sealed trait Move {
    def allowedMove(from: String): Pos
  }

  case object Left extends Move {
    def allowedMove(from: String) = Pos(-1, 0)
  }

  case object Right extends Move {
    def allowedMove(from: String) = Pos(+1, 0)
  }

  case object Down extends Move {
    def allowedMove(from: String) = Pos(0, +1)
  }

  case object Up extends Move {
    def allowedMove(from: String) = Pos(0, -1)
  }

  case object LeftAndRight extends Move {
    def allowedMove(from: String) =
      if (from == "RIGHT") Pos(-1, 0) else Pos(+1, 0)
  }

  case object LeftAndDown extends Move {
    def allowedMove(from: String) =
      if (from == "RIGHT") Pos(0,+1) else Pos(-1, 0)
  }

  case object RightAndDown extends Move {
    def allowedMove(from: String) =
      if (from == "LEFT") Pos(0, +1) else Pos(+1, 0)
  }

  case object None extends Move {
    def allowedMove(from: String) = Pos(+1, 0)
  }

  case class Cell(symbol: String, move: Move) {
    override def toString = symbol
  }

  val cellsTable = Map(
    0 -> Cell("░", None),
    1 -> Cell("╬", Down),
    2 -> Cell("═", LeftAndRight),
    3 -> Cell("║", Down),
    4 -> Cell("Y", LeftAndDown), // not implemented
    5 -> Cell("X", RightAndDown), // not implemented
    6 -> Cell("╩", LeftAndRight),
    7 -> Cell("╠", Down),
    8 -> Cell("╦", Down),
    9 -> Cell("╣", Down),
    10 -> Cell("╝", Left),
    11 -> Cell("╚", Right),
    12 -> Cell("╔", Down),
    13 -> Cell("╕", Down)
  )

  case class Bot(startPos: Pos, currentDirection:String) {
    var concurrentPos = startPos

    def nextMove(diff: Pos) = {
      Pos(concurrentPos.x + diff.x, concurrentPos.y + diff.y)
    }

    def goToNext(diff: Pos) = {
      concurrentPos = nextMove(diff)
    }
  }

  val Array(w, h) = for(i <- readLine split " ") yield i.toInt
  val data = for (i <- 0 until h) yield {
    val line = readLine.split(" ").map(item =>
      cellsTable(item.toInt))
    line
  }
  val ex = readInt // the coordinate along the X axis of the exit (not useful for this first mission, but must be read).

  while(true) {
    val Array(_xi, _yi, pos) = readLine split " "
    val xi = _xi.toInt
    val yi = _yi.toInt
    val botState = Bot(Pos(xi, yi), pos)
    Console.err.println( data(yi)(xi)) // just showing nice format of MAP
    val nextMove = botState.nextMove(
      data(yi)(xi).move.allowedMove(botState.currentDirection)
    )
    // One line containing the X Y coordinates of the room in which you believe Indy will be on the next turn.
    println(nextMove.x + " " + nextMove.y)

  }
}
