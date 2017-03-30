
/**
  * https://www.codingame.com/training/medium/there-is-no-spoon-episode-1
  * Created by kotobotov.ru on 06.03.2017.
  */
import math._
import scala.util._

/**
  * Don't let the machines win. You are humanity's last hope...
  **/

object Player extends App {
  val width = readInt // the number of cells on the X axis
  val height = readInt // the number of cells on the Y axis

  var data =for (i <- 0 until height) yield
    {readLine.split("").map(_ match { case "." => -1
    case _ => 0 })}

  def getCurrent(x: Int, y: Int) = {
    data(y)(x) match {
      case (-1) => "-1 -1"
      case (0) => s"$x $y"
    }
  }

  def getRight(x: Int, y: Int): String = try {
    getCurrent(x + 1, y) match {
      case "-1 -1" => getRight(x + 1, y)
      case output: String => output
    }
  } catch {
    case e: Exception => "-1 -1"
  }
  def getBottom(x: Int, y: Int): String = try {
    getCurrent(x, y + 1) match {
      case "-1 -1" =>
        getBottom(x, y + 1)
      case output: String => output
    }
  } catch {
    case e: Exception => "-1 -1"
  }

  def getNodes(x: Int, y: Int) = getCurrent(x, y) match {
    case "-1 -1" => ""
    case _ => s"$x $y ${getRight(x, y)} ${getBottom(x, y)}"
  }

  val result = for (i <- 0 until height; j <- 0 until width) yield getNodes(j, i)
  result
    .filter(_.nonEmpty)
    .foreach(println(_))
}
