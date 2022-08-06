import math._
import scala.util._
import scala.io.StdIn._
import scala.annotation.tailrec

/**
 * Created by Kotobotov on 01.06.2022
  * this solution is just using one tail recursive funcion - so its universal for any languages witch can call recorsive functions
  * i prefer this style becouse it's more generic and convinient way and can be easy transfer to any language (see sample for js, python and bash)
 */
object Solution extends App {
    val input = readLine.toList

def process(input: Seq[Char]):Boolean= {
  val openingBrackets = Set('{', '(', '[')
  val closingBrackets = Seq('}', ')', ']')
  val getClosingPartForBracket: Map[Char, Char] = Map('{' -> '}', '(' -> ')', '[' -> ']')
  @tailrec
  def helper(input: Seq[Char], stackWithOpenings:Seq[Char]): Seq[Char] = {
    input match {
      case Nil => stackWithOpenings
      case head :: tail if openingBrackets.contains(head) =>
        helper(tail, head +: stackWithOpenings)
      case head :: tail if closingBrackets.contains(head) =>
        if (stackWithOpenings.nonEmpty && getClosingPartForBracket.getOrElse(stackWithOpenings.head, '_') == (head)) {
          helper(tail, stackWithOpenings.tail)
        } else Seq('_')  // returning Seq('_') - just mean Error (terminating with nonEmpty processing)
      case head :: tail =>
        helper(tail, stackWithOpenings) // just ignoring head char
    }
  }

  helper(input, Seq.empty[Char]).isEmpty
}

println(process(input))

}