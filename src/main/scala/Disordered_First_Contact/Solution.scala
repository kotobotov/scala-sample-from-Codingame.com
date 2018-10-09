
/**
 * Created by Kotobotov.ru on 10.10.2018.
 */
import math._
import scala.util._
import scala.annotation.tailrec

object Solution extends App {
  def encode(input: String): String = {
    @tailrec
    def helper(in: List[Char], out: List[Char], take: Int): List[Char] = {
      in match {
        case Nil => out
        case _ if (take % 2 != 0) =>
          val (head, tail) = in.splitAt(take)
          helper(tail, out ++ head, take + 1)
        case _ if (take % 2 == 0) =>
          val (head, tail) = in.splitAt(take)
          helper(tail, head ++ out, take + 1)
      }
    }
    if (input.size == 0) "" else helper(input.toList, Nil, 1).mkString
  }

  def decode(input: String): String = {
    @tailrec
    def helper(left: List[Char],
               right: List[Char],
               target: List[Char],
               take: Int): List[Char] = {
      (left, right) match {
        case (Nil, Nil) => target
        case _ if (take % 2 != 0) =>
          val (targetLocalPart, rest) = right.splitAt(take)
          helper(left, rest, target ++ targetLocalPart, take + 1)
        case _ if (take % 2 == 0) =>
          val (rest, targetLocalPart) = left.splitAt(left.size - take)
          helper(rest, right, target ++ targetLocalPart, take + 1)
      }
    }
    val (left, right) = input.splitAt(normalizeSize(input.size) / 2)
    if (input.size == 0) ""
    else helper(left.toList, right.toList, Nil, 1).mkString
  }

  def normalizeSize(start: Int) = {
    var n = math.sqrt(start).floor
    def аrithmeticSum(n: Double) = (((n + 1.0) / 2.0) * n)
    while (аrithmeticSum(n) < start) {
      n += 1.0
    }
    аrithmeticSum(n).toInt
  }

  var count = readInt
  var message = readLine
  while (count != 0) {
    if (count > 0) {
      count -= 1
      message = decode(message)
    } else {
      count += 1
      message = encode(message)
    }
  }
  println(message)
}
