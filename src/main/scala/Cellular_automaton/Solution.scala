/**
  * Created by Kotobotov.ru on 04.10.2018.
  */
object Solution extends App {
  val response = String
    .format("%" + 8 + "s", readInt.toBinaryString)
    .replace(' ', '0')
    .replace("1", "@")
    .replace("0", ".")
  val rule = Map(
    "@@@" -> response(0),
    "@@." -> response(1),
    "@.@" -> response(2),
    "@.." -> response(3),
    ".@@" -> response(4),
    ".@." -> response(5),
    "..@" -> response(6),
    "..." -> response(7)
  )
  def makeNextPattern(input: String) =
    (input.last + input + input.head).sliding(3).toList.map(rule).mkString

  (0 until readInt).foldLeft(readLine)((acc, _) => {
    println(acc)
    makeNextPattern(acc)
  })
}
