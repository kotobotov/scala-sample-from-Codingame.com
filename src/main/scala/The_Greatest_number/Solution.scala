import math._
import scala.util._

/**
 * https://www.codingame.com/ide/puzzle/the-greatest-number
 * 	Goal

*You must print the greatest number using all the characters in the input, including the ten digits 0 to 9, the minus sign - and the decimal dot ..

*Beware:
* The dot alone must not end a number, at least a digit is needed. For example, 98742. is refused, write 9874.2 instead.
* Trailing and leading zeros must be removed. Write -4 instead of -4.0000 and -5.65 instead of-5.6500.
*Input
*Line 1 : The number N of chars
*Line 2: N chars on a single line separated with spaces
*Output
*The greatest number possible using all the input chars (but maybe without some zeros).
*Constraints
*1 ≤ N ≤ 10
*Example
*Input
*8
*4 9 8 . 8 5 2 -
*Output
*-2.45889
 **/
 
 
object Solution extends App {
    val n = readInt
    val input = readLine
var hasDemiter: Boolean = true
var sign: String = ""

var output = prepareData(input)
val result = printData(output.mkString(""))

def prepareData(rowInput: String) = {
    hasDemiter = rowInput.contains(".") 
   val stage1:List[String] = rowInput.contains("-") match {
        case true => sign = "-"
            rowInput.split(" ").toList.sorted
        case false => rowInput.split(" ").toList.sortWith(_>_)
    }
    val stage2 = stage1
            .filter(_!=".")
            .filter(_!="-")
    
    if (stage2.forall(_ == "0")) List("0") else stage2
}

def printData(input: String) = {
    input.size match {
        case 0 => println("")
        case 1 => if (input == "0") println(input) else println(sign + input)
        case _ => println(sign + {
            hasDemiter match {
                case true => addDemiter(input, sign)
                case false => input
            }
        })
    }
    def addDemiter(text: String, sign: String) = {
        sign match {
            case "-" => text.charAt(0) + "." + text.substring(1)
            case _ => text.substring(0, text.length - 1) + {
                text.charAt(text.length - 1) match {
                    case '0' => ""
                    case _ => "." + text.charAt(text.length - 1)
                }
            }
        }
    }
}
}
