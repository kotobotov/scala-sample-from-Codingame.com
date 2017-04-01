import scala.collection.mutable.ListBuffer

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/

// n: the total number of nodes in the level, including the gateways
// l: the number of links
// e: the number of exit gateways
val Array(n, l, exit) = for (i <- "4 4 1" split " ") yield i.toInt

val gateList = for (i<- (0 to n)) yield { new Gate(i) }


  gateList(0).connectNode(1)
  gateList(0).connectNode(2)
  gateList(1).connectNode(3)
  gateList(2).connectNode(3)

var exitList = List(3)
var traversingList = List.empty[Gate]
var needToTravers = List.empty[Gate]


// game loop
class Gate(_id: Int) {

  var status = "WHITE"
  var connectedTo = ListBuffer.empty[Int]
  val id: Int = _id

  def deleteNode(id: Int) = {connectedTo -= id }

  def connectNode(id: Int) = {connectedTo += id }

  def serchClosestConnectionTo(id: Int): Int = {10 }
}


var currentGate = new Gate(0)
var answer = 0
//while (true) {
  val si = 2 // The index of the node on which the Skynet agent is positioned this turn

  currentGate = gateList(si)
  answer = currentGate.connectedTo.find(item => exitList.contains(item)).getOrElse(currentGate.connectedTo.head)


  println(currentGate.id.toString + " " + answer)
//}


