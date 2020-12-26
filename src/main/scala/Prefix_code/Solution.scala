package Prefix_code

/**
 * Created by Dima Kubitskiy (@kotobotov) on 26.12.2020.
 */
import math._
import scala.util._
import scala.io.StdIn._
import scala.annotation.tailrec

object Solution extends App {
    val n = readLine.toInt
    val dictionary = (0 until n).map{item =>
        val Array(b, _c) = readLine split " "
        val c = _c.toInt
        b -> c.toChar
    }.sortBy(-_._1.size).toList


def decoder(input:String, matcher:List[(String, Char)]):String = {
  import scala.annotation.tailrec
  @tailrec
  def helpDecoder(input:String, currentMatcher:List[(String, Char)], stepCount:Int, acc:String):String= {
    input match {
      case in:String if in.isEmpty => acc   // terminal state for empty string
      case _:String if (currentMatcher.isEmpty) =>
        helpDecoder("", currentMatcher, stepCount, s"DECODE FAIL AT INDEX $stepCount") // terminal state for empty dictionary
      case in:String if in.startsWith(currentMatcher.head._1) =>
        helpDecoder(input.replaceFirst(currentMatcher.head._1, ""), matcher, stepCount+ currentMatcher.head._1.length, acc +  currentMatcher.head._2) // reduceFunction
      case _:String if (currentMatcher.tail.isEmpty) =>
        helpDecoder("", currentMatcher, stepCount, s"DECODE FAIL AT INDEX $stepCount") // terminal state for not having any decoder
      case in:String => helpDecoder(in, currentMatcher.tail, stepCount, acc) // just try next decoder
    }
   }
  helpDecoder(input, matcher, 0, "")
}

val codedData = readLine
val result = decoder(codedData, dictionary)
println(result)
}
