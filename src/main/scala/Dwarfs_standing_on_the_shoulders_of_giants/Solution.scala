/**
  * Created by Kotobotov.ru on 15.09.2018.
  */
import math._
import scala.util._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
object Solution extends App {
  val n = readInt // the number of relationships of influence
  val nodes = HashMap.empty[Int, ListBuffer[Int]]

  if (n == 0) println(0)

  for (i <- 0 until n) {
    val Array(x, y) = for (i <- readLine split " ") yield i.toInt
    if (!nodes.contains(x)) nodes += x -> ListBuffer()
    if (!nodes.contains(y)) nodes += y -> ListBuffer()
    nodes.get(x).map(node => node += y)
  }

  def maxPatchFrom(root: Int) = {
    val alredyTraversed = mutable.HashMap.empty[Int, Int]
    def dfs(node: Int, lenght: Int): Int = {
      if (nodes(node).isEmpty) lenght
      else {
        if (!alredyTraversed.contains(node))
          alredyTraversed += node -> nodes(node)
            .map(dfs(_, lenght + 1))
            .max
        alredyTraversed(node)
      }
    }
    dfs(root, 1)
  }

  val roots = nodes.keySet -- nodes.values.flatten.toSet
  val maxFromAllRoots = roots.map(maxPatchFrom).max

  println(maxFromAllRoots)
}
