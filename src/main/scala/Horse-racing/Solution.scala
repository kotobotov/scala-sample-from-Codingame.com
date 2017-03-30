

/**
  * https://www.codingame.com/training/easy/horse-racing-duals
  * Created by kotobotov.ru on 06.03.2017.
  */

object Solution extends App {
  val n = readInt
  val data = for (i <- 0 until n) yield readInt
  val result = data.sorted.sliding(2).map(item => item(1) - item(0)).min
  println(result)
}