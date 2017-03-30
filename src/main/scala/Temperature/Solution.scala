
import math._
import scala.util._

/**
 * 	The Goal - just solve task - more readeble approach than in CodeGolf solution
 https://www.codingame.com/ide/puzzle/temperatures

 **/
 
 
 
 
object Solution extends App {
    val n = readInt // the number of temperatures to analyse
    val temps = readLine // the n temperatures expressed as integers ranging from -273 to 5526

temps.length match {
    case 0 => println("0")
    case x:Int => println( temps.split(" ").map(_.toInt).toSeq.reduceLeft((a,b) =>
        if (abs(a)==abs(b)) a max b else Seq((a, (abs(a))), (b, (abs(b)))).minBy(_._2)._1))
}
}
