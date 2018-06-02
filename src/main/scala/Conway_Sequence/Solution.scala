package Conway_Sequence

/**
  * Created by Kotobotov.ru on 02.06.2018.
  * https://www.codingame.com/ide/puzzle/conway-sequence
  *
  */
import math._
import scala.util._
object Solution extends App {
  val r = readInt
  val l = readInt
  def look_count(input: List[Int]) = {

    def count(acc: Int, current: Int, elements: List[Int]): List[Int] = {
      elements match {
        case firstElementOfList :: rest => if (current == firstElementOfList) {
          count(acc + 1, firstElementOfList, rest)
        } else {
          acc :: current :: count(1, firstElementOfList, rest)
        }
        case Nil                       => acc :: current :: Nil
      }
    }
    count(1, input.head, input.tail)
  }

  def start(startElement: Int, lineNumber:Int) ={
    var currentElement:List[Int] = List(startElement)
    var counter  = 1
    while (lineNumber>counter){
      currentElement = look_count(currentElement)
      counter+=1
    }
    currentElement.mkString(" ")
  }
  println(start(r, l))
}
