package The_Gift

/**
  * Created by Kotobotov.ru on 02.06.2018.
  * https://www.codingame.com/ide/puzzle/the-gift
  *
  */
import math._
import scala.util._

object Solution extends App {
  val n = readInt
  val startBudget = readInt
  val budgetList =for(i <- 0 until n) yield readInt
  var restBudget = startBudget

  def count(input: List[Int]): List[Int] = input match {
    case currentBudged :: tail => val average = restBudget / (tail.size + 1)
      restBudget -= (if (currentBudged < average) currentBudged else average)
      if (currentBudged < average) currentBudged :: count(tail) else average :: count(tail)
    case Nil                   => Nil
  }
  val result = count(budgetList.sorted.toList)
  println(if (result.sum == startBudget) result.mkString("\n") else "IMPOSSIBLE")
}