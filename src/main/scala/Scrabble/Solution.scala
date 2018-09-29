/**
  * Created by Kotobotov.ru on 15.09.2018.
  */
object Solution extends App {
  val n = readInt
  val wordsDB = for {
    i <- 0 until n
    val w = readLine
  } yield w
  val letters = readLine

  val wordsHashed = wordsDB.toSet
  var memoryForOptimization = scala.collection.mutable.HashSet.empty[List[Char]]
  var resultContainer = scala.collection.mutable.HashSet.empty[String]

  // format: off
  val weight = Map('e'->1, 'a'->1, 'i'->1, 'o'->1, 'n'->1, 'r'->1, 't'->1, 'l'->1, 's'->1,'u'->1,
    'd'->2, 'g'->2,'b'->3, 'c'->3, 'm'->3, 'p'->3,'f'->4, 'h'->4, 'v'->4, 'w'->4, 'y'->4,'k'->5,
    'j'->8, 'x'->8,'q'->10, 'z'->10)
  // format: on
  def gererateWord(source: List[Char], sinck: List[Char]): Seq[List[Char]] = {
    (0 to source.length - 1)
      .map { item =>
        val (left, right) = source.splitAt(item)
        (right.head +: left ++: right.tail).toList
      }
      .map {
        case head :: Nil =>
          val result: List[Char] = sinck :+ head
          if (wordsHashed.contains(result.mkString))
            resultContainer += result.mkString
          result
        case head :: tail =>
          val result: List[Char] = sinck :+ head
          if (wordsHashed.contains(result.mkString))
            resultContainer += result.mkString
          if (memoryForOptimization.contains(result)) result
          else {
            memoryForOptimization += result
            gererateWord(tail, result)
            result
          }
        case Nil =>
          if (wordsHashed.contains(sinck.mkString))
            resultContainer += sinck.mkString
          sinck
      }
  }

  gererateWord(letters.toList, List.empty[Char])

  println(
    wordsDB
      .filter(word => resultContainer.contains(word)) // to not losing original ordering
      .maxBy(item => item.map(weight).sum)
  )
}
