/**
 * Created by Kotobotov.ru on 05.02.2019.
 */
object Solution extends App {
  class Point(val i: Int, val v: Int, val c: Int) {}
  def entropy(points: Array[Point]): Double = {
    val count = points.size.toDouble
    points.groupBy(_.c).map { case (k, v) =>
      val p = v.size / count
      -p * Math.log(p) / Math.log(2)
    }.sum
  }
  var lastSeparator: Int = 0
  class Node(val points: Array[Point]) {
    val count = points.size
    val currentEntropy = entropy(points)
    var smallerPoints: Array[Point] = null
    var largerPoints: Array[Point] = null
    var gain: Double = 0
    var separator: Point = points.head
    for(i <- 1 until count) {
      val point = points(i)
      val (tmpSmallerPoints, tmpLargerPoints) = points.partition(_.v < point.v)
      val weightedEntropy = tmpSmallerPoints.size * entropy(tmpSmallerPoints) / count +
                            tmpLargerPoints.size * entropy(tmpLargerPoints) / count
      if(weightedEntropy < currentEntropy + gain) {
        smallerPoints = tmpSmallerPoints
        largerPoints = tmpLargerPoints
        gain = weightedEntropy - currentEntropy
        separator = point
        lastSeparator = point.i
      }
    }
    if(smallerPoints != null) {
      new Node(smallerPoints)
      new Node(largerPoints)
    }
  }
  new Node((1 to readInt).map { i =>
    val Array(index, value, classi) = readLine split " " map(_.toInt)
    new Point(index, value, classi)
  }.toArray)
  println(lastSeparator)
}
