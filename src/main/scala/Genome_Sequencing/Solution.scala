/**
  * Created by Kotobotov.ru on 17.10.2018.
  */
import scala.annotation.tailrec
import scala.collection.mutable

object Solution extends App {
  val n = readInt - 1
  val genoms = (0 to n).permutations.toList

  val genomCode = for (i <- (0 to n)) yield readLine
//val genomCode = Seq("BAG", "GAA", "TACAGA")

  case class MerjedWords(left: String, right: String)

  val memoize = mutable.HashMap.empty[MerjedWords, Int]

  def mergeByIndex(left: Int, right: Int): Int = {
    def calculateMinMergeSize(wordsPair: MerjedWords): Int = {
      @tailrec
      def helper(in: List[Char], target: List[Char], result: Int): Int = {
        val result = if ((in zip target).forall {
          case (left, right) => left == right
        }) in.size min target.size
                     else 0

        in match {
          case Nil               => result
          case _ if (result > 0) => result
          case _ :: tail         => helper(tail, target, result)
        }
      }
      helper(wordsPair.left.toList, wordsPair.right.toList, 0)
    }

    val wordsPair = MerjedWords(genomCode(left), genomCode(right))
    if (memoize.contains(wordsPair)) memoize(wordsPair)
    else {
      val minMerjedSize = calculateMinMergeSize(wordsPair)
      memoize += wordsPair -> minMerjedSize
      minMerjedSize
    }
  }

  val maxMergedParts = if(n<=0) 0 else genoms
                                       .map(_.sliding(2).collect {
                                         case Vector(a, b) => mergeByIndex(a, b)
                                       }.sum)
                                       .max

  println(genomCode.map(_.length).sum - maxMergedParts )

}
