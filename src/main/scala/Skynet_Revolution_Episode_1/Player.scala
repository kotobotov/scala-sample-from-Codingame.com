import scala.collection.mutable.ListBuffer

object Player extends App {
  // n: the total number of nodes in the level, including the gateways
  // l: the number of links
  // e: the number of exit gateways
  val Array(n, l, exit) = for (i <- readLine split " ") yield i.toInt
  val gateList = for (i<- (0 to n)) yield { new Gate(i) }
  for (i <- 0 until l) {
    // n1: N1 and N2 defines a link between these nodes
    val Array(n1, n2) = for (i <- readLine split " ") yield i.toInt
    gateList(n1).connectNode(n2)
    gateList(n2).connectNode(n1)
  }
  val exitList = for (i <- 0 until exit) yield readInt // the index of a gateway node
  class Gate(_id: Int) {
    var status = "WHITE"
    var connectedTo = ListBuffer.empty[Int]
    val id: Int = _id
    def deleteNode(id: Int) = connectedTo -= id
    def connectNode(id: Int) = connectedTo += id
  }

  var skyNetPosition = new Gate(0)
  var defendNodeId = 0
  while (true) {
    skyNetPosition = gateList(readInt)
    defendNodeId = skyNetPosition.connectedTo.find(item => exitList.contains(item)).getOrElse(skyNetPosition.connectedTo.head)
    println(skyNetPosition.id.toString + " " + defendNodeId)
  }
}

