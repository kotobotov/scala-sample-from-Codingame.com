

/**
 * Created by Kotobotov.ru on 15.09.2018.
 */

object Solution extends App {
  val n = readInt


  var acc = 0
  var step = 1

  def sumOfSteps(input: Int): Int = {
    if (input <= acc) step else {
      acc += step
      step += 1
      sumOfSteps(input - 1)
    }
  }
  println(sumOfSteps(n-1))
}