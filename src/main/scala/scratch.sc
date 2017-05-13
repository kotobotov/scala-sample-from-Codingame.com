import scala.math._

//val data = scala.io.Source.fromFile("C:\\Users\\Администратор.WIN-F18M6U14154\\Downloads\\BukvarixSetup1.1\\Bukvarix1.1\\Data\\data.dat")("UTF-8").getLines().take(2).toList
//data.foreach(println)
val grid = Array.ofDim[Double](23, 21)
val Y = ("1 3 4 5 4 7 10 8 12" split " ").map(_.toInt)
val W = ("1 3 4 5 4 7 10 8 12" split " ").map(_.toInt)

case class Result(max: Int, p: Int)

//val init = Result(input.head, 0)
//val result = input.foldLeft(init) { (acc, value) =>
//  Result(
//    max = max(acc.max, value),
//    p = min(acc.p, value - acc.max)
//  )
//}

implicit class Data(input: String) {
  def toJson = "{ \"data\" : " + input + " }"
}

implicit class Neyron(input:Array[Int]){
  def *(in:Array[Int]) = {}
}


val X = Y * W

//Neyron z = new Neyron(Y)
//
//z.mult()

"10" toJson
//Y = k*X - pri etom Y eto seqOfInt
//new Y(inicializacia).connect new Neyron(X).init(dataToInit).mult({() -> matrica.mult(k)})