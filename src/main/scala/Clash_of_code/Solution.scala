package Clash_of_code

/**
  * Created by Kotobotov.ru on 27.04.2017.
  */
object Solution {
  // GET MID IN DATA
  val n = readInt

  def getMid(data: Array[Int]) = {
    data.filter(_ != data.max)
      .filter(_ != data.min)
      .head
  }

  val inputData = for (i <- 0 until n) yield {
    val input = for (i <- readLine split " ") yield i.toInt
    input
  }
  inputData.map(getMid(_))
    .foreach(println)
}


object Solution2 {
  // FIND REPITEBLE VOWELS OR NOT VOWELS
  "a b c d e f g h i j k l m n o p q r s t u v w x y z"
  val vovel = "a e i o u y".split(" ").toSet
  val glassnie = "b c d f g h j k l m n p q r s t v w x z".split(" ").toSet
  val data = "sovo"

  val input = data.map(item => vovel.contains(item.toString)).toList

  def getAnswer(input: List[Boolean]): Boolean = {
    input match {
      case x1 :: x2 :: xs if (x1 == x2) => true
      case x1 :: x2 :: xs => getAnswer(x2 :: xs)
      case x1 :: Nil => false
    }
  }

  getAnswer(input)
}

object Solution3 {
  // print FOO or BAR in generated sequens
  val n = readInt
  var data = (1 to n).map(_.toString).toArray
  val by5 = (n - 1) / 5
  val by7 = (n - 1) / 7
  if (by5 >= 1) {
    for (i <- (1 to by5)) {
      data(i * 5 - 1) = "Foo"
    }
    for (j <- (1 to by7)) {
      if (data(j * 7 - 1) == "Foo") data(j * 7 - 1) = "FooBar" else data(j * 7 - 1) = "Bar"
    }
  }
  data
}
object Solution4 {
  // get chars of word to int and average to char again
  val seqWords = readLine.toUpperCase.split(" ").map { word =>
    word.toCharArray.map(_.toInt).sum / word.size
  }
  seqWords.map(_.toChar).foreach(println)
}