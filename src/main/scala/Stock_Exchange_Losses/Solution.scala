import scala.annotation.tailrec

/**
  * Created by Kotobotov.ru on 03.04.2017.
  */
object Solution extends App {
  val n = readInt
  var inputs = readLine split " "
  val data = inputs.map(_.toInt).toList

  @tailrec def getMin(data: List[Int], acc: Int, lastMin: Int): Int = {
    data match {
      case x :: Nil => acc
      case x :: xs if (x == lastMin) =>  // buffering minValue to increase speed, recalculate min only if drop lastMin value
        getMin(xs, acc, xs.min)
      case x :: xs =>
        val segmentSize = x - lastMin
        if (segmentSize > acc) getMin(xs, segmentSize, lastMin) else getMin(xs, acc, lastMin)
    }
  }

  println(getMin(data, 0, data.min) * (-1))
}
