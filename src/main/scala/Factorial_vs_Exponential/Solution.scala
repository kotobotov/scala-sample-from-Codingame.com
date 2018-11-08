/**
 * Created by Kotobotov.ru on 08.11.2018.
 */

object Solution extends App {
  val k = readInt
  var inputs = readLine split " "
  val solution = inputs.map(_.toDouble).map(solver)
  def solver(in: Double) = {
    val logA = math.log(in)
    var sum = 0.0
    var i = 1
    while (sum <= i * logA) {
      i += 1
      sum += math.log(i)
    }
    i
  }
  println(solution.mkString(" "))
}
