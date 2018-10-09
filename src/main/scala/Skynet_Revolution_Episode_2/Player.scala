
/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  // n: the total number of nodes in the level, including the gateways
  // l: the number of links
  // e: the number of exit gateways
  val Array(n, l, exit) = for (i <- readLine split " ") yield i.toInt
  for (i <- 0 until l) {
    // n1: N1 and N2 defines a link between these nodes
    val Array(n1, n2) = for (i <- readLine split " ") yield i.toInt
  }
  for (i <- 0 until exit) {
    val ei = readInt // the index of a gateway node
  }


  var traversingList = List.empty[Gate]
  var needToTravers = List.empty[Gate]

  def traversing(currentList: List[Gate]) = {
    currentList.reduce { (item1, item2) =>
      item1 :: item2.getNotTraversedNode()
    }.head match {
      case x: Gate if (x.id == 1)
    }
  }

  // game loop
  class Gate(_id: Int) {
    def getNotTraversedNode() = {
      connectedTo.filter(_.status == "WHITE")
    }

    var status = "WHITE"
    var connectedTo = List.empty[Gate]
    val id: Int = _id

    def deleteNode(id: Int) = {}

    def connectNode(id: Int) = {}

    def serchClosestConnectionTo(id: Int): Int = {10 }
  }

  def chooseClosestExitToSkyNet() = {
    var exitList = List.empty[Gate]
    exitList.head
  }

  while (true) {
    val si = readInt // The index of the node on which the Skynet agent is positioned this turn
    exit match {
      case 1 =>
      case _ => chooseClosestExitToSkyNet()
    }
    val toDelete = chooseClosestExitToSkyNet().serchClosestConnectionTo(si)
    chooseClosestExitToSkyNet().deleteNode(toDelete)
    println(chooseClosestExitToSkyNet().id + " " + toDelete)
  }
}

