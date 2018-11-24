/**
  * Created by Kotobotov.ru on 25.11.2018.
  */
object Solution extends App {
  val n = readInt
  val input = (for (i <- 0 until n) yield readLine).mkString("")
  var isNeedNewLine = false
  var firstLine = true
  var isInString = false
  var blockCounter = 0
  def indent = (0 until blockCounter * 4).map(item => " ").mkString
  def newLine = if (isNeedNewLine) "\n" + indent else ""
  def addNewBlock = blockCounter += 1
  def exitBlock = blockCounter -= 1
  def startNewBlock(c: Char) = {
    isNeedNewLine = true
    val beforeBlock = newLine + c
    addNewBlock
    if (firstLine) {
      firstLine = false
      c
    } else beforeBlock
  }
  def exitFromBlock(c: Char) = {
    isNeedNewLine = true
    exitBlock
    newLine + c
  }

  def newLineCurrentBlock(c: Char) = {
    isNeedNewLine = true
    c.toString
  }
  def currentBlockContent(c: Char) = {
    firstLine = false
    val content = newLine + c
    isNeedNewLine = false
    content
  }

  def prepare(input: List[Char]): String = {
    if (!isInString) input match {
      case head :: rest if head == '(' =>
        startNewBlock(head) + prepare(rest)
      case head :: rest if head == ')' =>
        exitFromBlock(head) + prepare(rest)
      case head :: rest if head == ';' =>
        newLineCurrentBlock(head) + prepare(rest)
      case head :: rest if head == '\'' =>
        isInString = true
        currentBlockContent(head) + prepare(rest)
      case head :: rest if (head == '\n' || head == ' ' || head == '\t') =>
        prepare(rest)
      case head :: rest =>
        currentBlockContent(head) + prepare(rest)
      case Nil => ""
    } else
      input match {
        case head :: rest if head == '\'' =>
          isInString = false
          head + prepare(rest)
        case head :: rest =>
          head + prepare(rest)
        case Nil => ""
      }
  }

  println(prepare(input.toList))
}
