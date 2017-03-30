/**
  * The Goal - just solve
  * https://www.codingame.com/ide/puzzle/chuck-norris
  * Created by kotobotov.ru on 28.03.2017.
  */


object Solution extends App {
  val message = readLine

  def mkBool(inp: Char) = ("%07d".format(inp.toBinaryString.toInt)).mkString

  var state = '?'

  def same(input: Char) = input match {
    case x if (x == state) => true
    case _ => state = input
      false
  }

  def mkChack(inp: String) = inp.toList.map({
    case '0' => if (same(0)) "0" else " 00 0"
    case '1' => if (same(1)) "0" else " 0 0"
  }).mkString.trim

  print(mkChack(message.map(mkBool(_)).mkString))
}
