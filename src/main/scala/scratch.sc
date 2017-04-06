import math._
var input = ("1 3 4 5 4 7 10 8 12" split " ").map(_.toInt).toList

  case class Result(max: Int, p: Int)
  val init = Result(input.head, 0)
  val result = input.foldLeft(init) { (acc, value) =>
    Result(
      max = max(acc.max, value),
      p = min(acc.p, value - acc.max)
    )
  }
  println(result.p)
