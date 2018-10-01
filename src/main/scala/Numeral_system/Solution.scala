
/**
 * Created by Kotobotov.ru on 01.10.2018.
 */
object Solution extends App {
  val input = readLine
  Console.err.println(input)
  val  dataChars = Map('0'->0, '1'->1, '2'->2, '3'->3, '4'->4,'5'->5,'6'->6,'7'->7, '8'->8,'9'->9,
    'A' -> 10, 'B'->11, 'C'-> 12, 'D'-> 13, 'E'-> 14, 'F'->15, 'G'->16, 'H'->17, 'I'->18, 'J'->19,
    'K'->20, 'L'->21, 'M'->22, 'N'->23, 'O'->24, 'P'->25, 'Q'->26, 'R'->27, 'S'->28, 'T'->29, 'U'->30,
    'V'->31, 'W'->32, 'X'->33, 'Y'->34, 'Z'->35)
  val baseSystem = (2 to 37)
  def fromBase(input:String,base:Int)=
  {
    input.reverse.split("")
    .zipWithIndex
    .map(item => dataChars(item._1.head)*math.pow(base, item._2))
    .sum.toLong
  }
  def valid(input:String,base:Int) = input.split("[+=]").flatten.forall(item => dataChars(item)<base)
  def convert(input:String, base:Int) = input.split("[+=]")
                                        .map(item =>fromBase(item, base))

  val result = for {
    base <- baseSystem
    Array(x, y, z) = convert(input, base) if valid(input, base) && x+y==z
  } yield base

  println(result.min)
}
