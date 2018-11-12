/**
 * Created by Kotobotov.ru on 13.11.2018.
 */

object Solution extends App {
  val input = readLine


  implicit class LeterChecker(in: Char) {
    def punctuation = !in.isLetterOrDigit && !in.isSpaceChar
  }

  val singleSpace = (acc: List[Char], elem: Char) => {
    if (elem == ' ' && acc.nonEmpty && acc.head == ' ')  acc
    else elem :: acc
  }

  val noSpaceBefore = (elem: Char, acc: List[Char]) => {
    if (elem.punctuation && acc.nonEmpty && acc.head == ' ' && (acc.tail.head.isLetter || acc.tail.head == elem))
      elem :: acc.tail
    else elem :: acc
  }
  val oneSpaceAfter = (elem: Char, acc: List[Char]) => {
    if (elem.isLetter && acc.nonEmpty && acc.head.punctuation) elem :: ' ' :: acc
    else elem :: acc
  }
  val makeUpperFirstLetter = (elem: Char, acc: List[Char]) => {
    if (acc.nonEmpty && acc.head == ' ' && acc.tail.nonEmpty && acc.tail.head == '.')
      elem.toUpper :: acc
    else if (acc.isEmpty) elem.toUpper :: acc
    else elem :: acc
  }
  val removeRepited = (elem: Char, acc: List[Char]) => {
    if (elem.punctuation && acc.head == elem) acc
    else elem :: acc
  }

  val solution =
    (List.empty[Char] /: input.toLowerCase.toList)(singleSpace)
    .foldRight(List.empty[Char])(noSpaceBefore)
    .foldRight(List.empty[Char])(removeRepited)
    .foldRight(List.empty[Char])(oneSpaceAfter)
    .foldRight(List.empty[Char])(makeUpperFirstLetter)
    .reverse
    .mkString
  println(solution)
}
