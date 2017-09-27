
/**
  * Created by Kotobotov.ru on 27.09.2017.
  */

object Solution extends App {
  val n = readInt

  def isSafe(col: Int, queens: List[Int]):Boolean = {
    val row = queens.length
    val queensWithRow = (row -1 to 0 by -1) zip queens // all queens 2d coordinat on board
    queensWithRow forall{
      case(r, c) => col != c && math.abs(col-c) != row-r // check for new queen are not been beaten
    }
  }


  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    placeQueens(n)
  }


  def show(queens:List[Int]) = {
    val lines =
      for(col <- queens.reverse) yield Vector.fill(queens.length)("- ").updated(col, "Q ").mkString
    "\n" + (lines mkString "\n")
  }

//Queens placement can be rendered as:
//Console.err.println( queens(n).take(3) map show )

  println(queens(n).size)
}
