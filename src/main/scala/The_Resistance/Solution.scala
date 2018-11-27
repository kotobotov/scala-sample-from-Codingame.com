/**
  * Created by Kotobotov.ru on 27.11.2018.
  */
object Solution extends App {
  val (input,n) = (readLine, readInt)
  val words = for(i <- 0 until n) yield readLine
  val alphabit = Map(
    'A' -> ".-", 'B' -> "-...", 'C' -> "-.-.", 'D' -> "-..", 'E' -> ".", 'F' -> "..-.",
    'G' -> "--.", 'H' -> "....", 'I' -> "..", 'J' -> ".---", 'K' -> "-.-", 'L' -> ".-..",
    'M' -> "--", 'N' -> "-.", 'O' -> "---", 'P' -> ".--.", 'Q' -> "--.-", 'R' -> ".-.",
    'S' -> "...", 'T' -> "-", 'U' -> "..-", 'V' -> "...-", 'W' -> ".--", 'X' -> "-..-",
    'Y' -> "-.--", 'Z' -> "--..")

  val transformedWords = words
                         .map(item => item.map(char => alphabit(char)).mkString)
                         .map(item => (item, 1))
                         .groupBy(_._1)
                         .mapValues(_.size)

  val wordsSizes = transformedWords.keys.map(item => item.length).toList

  val TARGET_SIZE = input.size
  var memoize = Map.empty[Int, Long]

  def count(startFrom: Int): Long = {
    startFrom match {
      case TARGET_SIZE                      => 1L
      case start if memoize.contains(start) => memoize(start)
      case start                            => val sumOfCombination: Long =
        (for {nextPosition <- wordsSizes if (startFrom + nextPosition) <= TARGET_SIZE
              result = if (transformedWords.contains(input.substring(startFrom, startFrom + nextPosition)))
                         transformedWords(input.substring(startFrom, startFrom + nextPosition)
                         ) * count(startFrom + nextPosition) else 0
        } yield result).sum
        memoize += (start -> sumOfCombination)
        sumOfCombination
    }
  }
  println(count(0))
}