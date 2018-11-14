/**
 * Created by Kotobotov.ru on 14.11.2018.
 */
object Solution extends App {
  val input = readLine.toList

  val startElements = Map('(' -> ')', '[' -> ']', '{' -> '}')
  val endElements = Map(')' -> '(', ']' -> '[', '}' -> '{')
  val allowedBrackets = Set('(', ')', '[', ']', '{', '}')

  def stateMachine(opened:List[Char], in:Char):List[Char] = {
    in match {
      case current if !allowedBrackets.contains(current) => opened
      case current if startElements.contains(current) => current::opened
      case current if (opened.nonEmpty && opened.head == endElements(current)) => opened.tail
      case _ => '%'::Nil // just mark erorr as % symbol
    }
  }

  val solution = input.foldLeft(List.empty[Char])(stateMachine)

  println(solution.isEmpty)
}