/**
 * Created by Kotobotov.ru on 01.10.2018.
 */
object Solution extends App {
  val r = readInt
  val v = readInt
  val data = for{
    j <- 0 until v
    Array(c, n) = readLine split " "
  } yield (c.toInt, n.toInt)

  def addToMin(acc:Seq[Double],elem:Double):Seq[Double]={
    acc.sorted match {
      case head::tail => head  + elem :: tail
    }
  }
  val answer = data
               .map{case(c, n) => math.pow(10,n) * math.pow(5, c - n)}
               .foldLeft(Seq.fill[Double](r)(0.0))(addToMin)
               .max

  println(answer.toInt)
}
