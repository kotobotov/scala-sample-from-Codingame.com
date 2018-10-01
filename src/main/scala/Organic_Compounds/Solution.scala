/**
 * Created by Kotobotov.ru on 01.10.2018.
 */

object Solution extends App {
  val n = readInt
  val rowString= (for(i <- 0 until n) yield readLine).toArray
  val maxElementsSize = rowString.map(_.size / 3).max

  sealed trait Carbon extends Product with Serializable {
    var allowedLinks = 0
  }
  case class CH(h: Int) extends Carbon {
    allowedLinks = 4 - h
  }
  case class Link(input: Int) extends Carbon {
    allowedLinks = 0
  }
  case object NoLink extends Carbon {
    allowedLinks = 0
  }

  def toCarbon(input: String) =
    if (input.startsWith("CH")) CH(input.substring(2).toInt)
    else if (input.startsWith("("))
           Link(
             input
             .replace("(", "")
             .replace(")", "")
             .toInt
           )
    else NoLink

  val preparedDate: Array[Array[Carbon]] =
    rowString.map { item =>
      val result = item
                   .sliding(3, 3)
                   .map(toCarbon)
                   .toArray
      result ++ Array.fill[Carbon](maxElementsSize - result.size)(NoLink) // in order to create Square Matrix
    }.toArray ++ Array(Array.fill[Carbon](maxElementsSize)(NoLink)) // need at least 2 argument (so 2x2 matrix - minimum)

  def validationTest(input: Array[Array[Carbon]]) = {
    def eval(arg1: Carbon, arg2: Carbon) = { (arg1, arg2)
       match {
        case (Link(n), _) =>
          arg2.allowedLinks -= n
        case (_, Link(n))=>
          arg1.allowedLinks -= n
        case _ =>
      }
    }
    def calculateAllowedLinks(input: Array[Array[Carbon]]) = {
      input
      .sliding(1, 2)
      .toArray
      .flatten
      .foreach(
        line => line.sliding(2, 1).foreach { case Array(a, b) => eval(a, b) }
      )
    }

    calculateAllowedLinks(preparedDate)
    calculateAllowedLinks(preparedDate.transpose)

    preparedDate.map(item => item.map(_.allowedLinks)).flatten.forall(_ == 0)
  }

  val isValid = validationTest(preparedDate)
  println(if (isValid) "VALID" else "INVALID")

}
