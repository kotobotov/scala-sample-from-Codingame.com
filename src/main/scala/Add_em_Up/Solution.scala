/**
 * Created by Kotobotov.ru on 04.12.2018.
 */

import scala.collection.mutable.PriorityQueue
object Solution extends App {
  readInt
  val input = readLine.split(" ").map(_.toInt)
  var q = PriorityQueue(input: _*)(Ordering.by(item => -item))
  println(input.foldLeft(0)((summ, _) => {
    val left = q.dequeue()
    if (q.isEmpty) summ else {
      val intermidiat = q.dequeue() + left
      q += intermidiat
      intermidiat + summ
    }
  }))
}
