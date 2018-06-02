/**
  * Created by Kotobotov.ru on 02.06.2018.
  * https://www.codingame.com/training/medium/telephone-numbers
  */
import math._
import scala.util._
import scala.collection.mutable.Map
class Node(var currentChars: Map[Char, Node]) {
  def add(phoneNumber: List[Char]): Node = {
    phoneNumber match {
      case head :: tail => if (currentChars.contains(head)) {
        currentChars(head).add(tail)
        currentChars(head)
      } else {
        currentChars += (head -> new Node(Map.empty[Char, Node]))
        currentChars(head).add(tail)
        currentChars(head)
      }
      case Nil          => new Node(Map.empty[Char, Node])
    }
    new Node(Map.empty[Char,Node])
  }

  override def toString: String = currentChars.keys.mkString(" ") + currentChars.values.map(_.toString)
  def count:Int = currentChars.keys.size + currentChars.values.map(_.count).sum
}


object Solution extends App {
  val dataDB = new Node(Map.empty[Char, Node])
  val n = readInt
  val inputPhones = for(i <- 0 until n) yield readLine.split("").map(_.head).toList
  inputPhones.foreach(phoneNumber => dataDB.add(phoneNumber))
  println(dataDB.count)
}
