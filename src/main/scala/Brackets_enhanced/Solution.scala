/**
  * Created by Kotobotov.ru on 16.11.2018.
  * based on Brackets_extreme solution
  */
object Solution extends App {

  val swap =
    Map('(' -> ')', '[' -> ']', '{' -> '}', ')' -> '(', ']' -> '[', '}' -> '{')
  val startElements = Map('(' -> ')', '[' -> ']', '{' -> '}')
  val endElements = Map(')' -> '(', ']' -> '[', '}' -> '{')
  val allowedBrackets = Set('(', ')', '[', ']', '{', '}')

  def stateMachine(opened: List[Char], in: Char): List[Char] = {
    in match {
      case current if !allowedBrackets.contains(current) => opened
      case current if startElements.contains(current)    => current :: opened
      case current
          if (opened.nonEmpty && opened.head == endElements(current)) =>
        opened.tail
      case _ => '%' :: Nil // just mark erorr as % symbol
    }
  }

  val solution = "({})".toList
    .foldLeft(List.empty[Char])(stateMachine)
//createBracet(startElements(in.head), in.tail)
  println(solution.isEmpty)

  val source = List('(', '{', '[')
  val allBrakets: Stream[List[Char]] = source #:: allBrakets

  sealed trait Node
  case class NonEmpty(char: Char, left: Node, right: Node) extends Node {
    override def toString = s"($char, \n ${left.toString} , ${right.toString} )"
  }
  case object EmptyLeaf extends Node {
    override def toString = ""
  }

  var counter = 0

  def create(input: List[Char], opened: List[Char]): Node = {
    input match {
      case Nil => EmptyLeaf
      case x :: xs =>
        counter += 1
        val newOpenedBrackets = stateMachine(opened, x)
        if (newOpenedBrackets.nonEmpty && newOpenedBrackets == '%')
          EmptyLeaf
        else
          NonEmpty(
            x,
            create(xs, newOpenedBrackets),
            if (xs.isEmpty) EmptyLeaf
            else
              create(swap(xs.head) :: xs.tail, newOpenedBrackets)
          )
    }
  }

  create(source, List.empty[Char])
  counter
}
