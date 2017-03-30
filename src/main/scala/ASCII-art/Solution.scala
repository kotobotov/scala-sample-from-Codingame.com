/**
  * *
  * The Goal - just solve
  * https://www.codingame.com/ide/puzzle/ascii-art
  * Created by kotobotov.ru on 28.03.2017.
  **/

object Solution extends App {
  val l = readInt
  val h = readInt
  val t = readLine
  val row = for (i <- 0 until h) yield readLine
  val sample = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"

  def getChar(index: Int): IndexedSeq[String] = index match {
    case x if x >= 0 => row.map(_.substring((index * l), (index * l + l)))
    case _ => getChar(sample.indexOf("?"))
  }

  val text = t.map(char => getChar(sample.indexOf(char.toUpper)))
  val result = text.reduceLeft { (a, b) => a.zip(b).map(item => item._1 + item._2) }
  result.foreach(println(_))
}

