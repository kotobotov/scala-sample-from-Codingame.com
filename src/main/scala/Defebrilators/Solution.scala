

/**
  * https://www.codingame.com/training/easy/defibrillators
  * Created by kotobotov.ru on 06.03.2017.
  */

object Solution extends App {
  val lon = toStandart(readLine)
  val lat = toStandart(readLine)
  val n = readInt
  val result = defibrilators.minBy(defib => position(defib.longitude, defib.latitude)).name
  var rowData = Array.empty[String]
  var defibrilators = for (i <- 0 until n; rowData = readLine.split(";")) yield Defibrilator(rowData(0).toInt, rowData(1), rowData(2), rowData(3), toStandart(rowData(4)), toStandart(rowData(5)))

  def toStandart(input: String): Double = {
    math.toRadians(input.replace(",", ".").toDouble)
  }

  def position(longitude: Double, latitude: Double) = {
    val x = (longitude - lon) * math.cos((lon + longitude) / 2)
    val y = (latitude - lat)
    math.sqrt(x * x + y * y) * 6371 // from formula
  }

  case class Defibrilator(id: Int, name: String, addres: String, phone: String, longitude: Double, latitude: Double)

  println(result)
}