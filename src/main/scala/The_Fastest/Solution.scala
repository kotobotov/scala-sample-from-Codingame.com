import math._
import scala.util._

/**
Community pazle: https://www.codingame.com/ide/puzzle/the-fastest (50 pts)

The program:
Your program must judge results of marathon runners and choose the best one.

The result of each runner is represented as HH:MM:SS, where HH is hours, MM is minutes and SS is seconds.

You are given N results and the smallest time is the best.

INPUT:
Line 1: a integer number N.
Next N lines: 8 characters, a time result.

OUTPUT:
The best result.

CONSTRAINTS:
0 < N ≤ 10
0 ≤ hours < 24
0 ≤ minutes < 60
0 ≤ seconds < 60
 **/
 
 
object Solution extends App {
    val n = readInt
var rowData = List[String]()
    for(i <- 0 until n) {
        val t = readLine
        rowData = rowData ++ List(t)
    }
    
val data = rowData.map(_.split(":").map(_.toInt).toList).map(_ match {
    case List(a, b, c) => (a, b, c)
})
val stage1 = data.filter(_._1 == data.min._1)
val stage2  = stage1.filter(_._2 == stage1.min._2)
val result  = stage2.filter(_._3 == stage2.min._3).head

println(pritty(result._1.toString)+":"+pritty(result._2.toString)+":"+pritty(result._3.toString))

def pritty(input: String) = input.size match {
    case 1 => 0 + input
    case 2 => input
}

}
