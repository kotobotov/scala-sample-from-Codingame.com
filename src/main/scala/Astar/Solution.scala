import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Kotobotov.ru on 26.08.2018.
  */
class IndexMinPQ[Key, Value](initialSize: Int)(implicit ord: Ordering[Value]) {

  /**
    * @param value The external Value type
    * @param index index in the binary heap
    */
  sealed case class Node(var value: Value, var index: Int)
  val index = new mutable.OpenHashMap[Key, Node](initialSize)
  // idx 0 is not used, bitcount is level, root idx is 1, left sibling 10, right 11, and so on
  val heap = new ArrayBuffer[Key](initialSize + 1)
  //def defaultValue[U]: U = { class Default[U] {var default: U = _ }; new Default[U].default }
  //heap += defaultValue[Key] // dummy element
  //heap += 0.asInstanceOf[Key] null causes intellij not to display heap correctly
  heap += null.asInstanceOf[Key]

  def length: Int = heap.length - 1
  def isEmpty: Boolean = length == 0
  def nonEmpty: Boolean = length != 0

  private[this] val rootIndex = 1
  private[this] def parentIndex(index: Int): Int = {
    require(index > 0)
    require(index < heap.size)
    index >>> 1
  }
  private[this] def leftIndex(index: Int): Int = {
    (index << 1)
  }
  private[this] def rightIndex(index: Int): Int = (index << 1) + 1
  private[this] def leftSiblingIndex(index: Int) = {
    require((index & 1) == 1)
    index - 1
  }
  private[this] def rightSiblingIndex(index: Int) = {
    require((index & 1) == 0)
    index + 1
  }

  private[this] def getNodeAt(extract: (Int) => Int, idx: Int): Option[Node] = {
    val target = extract(idx)
    if (target < heap.size) {
      val nodeOpt = index.get(heap(target))
      assert(nodeOpt.nonEmpty)
      nodeOpt
    } else
        None
  }

  private[this] def parentHeap(node: Node): Option[Node] = {
    node.index match {
      case 1      => None
      case _ @idx => index.get(heap(parentIndex(idx)))
    }
  }
  private[this] def leftHeap(node: Node): Option[Node] =
    getNodeAt(leftIndex, node.index)
  private[this] def rightHeap(node: Node): Option[Node] =
    getNodeAt(rightIndex, node.index)
  private[this] def leftSiblingHeap(node: Node): Option[Node] =
    getNodeAt(leftSiblingIndex, node.index)
  private[this] def rightSiblingHeap(node: Node): Option[Node] =
    getNodeAt(rightSiblingIndex, node.index)

  /**
    * Swap two nodes in the heap and changing the respective Node indices
    */
  def swapHeap(from: Node, to: Node): Unit = {
    swapKey(from.index, to.index)
    // swap index
    val oldToIdx = to.index
    to.index = from.index
    from.index = oldToIdx
  }

  private[this] def swapKey(aIdx: Int, bIdx: Int): Unit = {
    val toKey = heap(bIdx)
    heap(bIdx) = heap(aIdx)
    heap(aIdx) = toKey
  }

  @tailrec
  private[this] def downHeap(node: Node): Unit = {
    val left = leftHeap(node)
    if (left.nonEmpty) {
      val right = rightHeap(node)
      val target =
        if (right.nonEmpty && ord.lt(right.get.value, left.get.value)) right.get
        else left.get
      if (ord.lt(target.value, node.value)) {
        swapHeap(node, target)
        downHeap(node)
      }
      // else we found our place
    }
    // else we found our place
  }

  @tailrec
  private[this] def upHeap(node: Node): Unit = parentHeap(node) match {
    case Some(parent) if (ord.lt(node.value, parent.value)) => {
      // restore heap property with parent
      swapHeap(node, parent)
      upHeap(node)
    }
    case None => // this is the root, stop
    case _    => // property heap holds, stop
  }

  def assertHeap: Unit = {
    heap.iterator.zipWithIndex.drop(2).foreach { x =>
      val idx = x._2
      val k = x._1
      val node: Node = index(k)
      if (ord.gt(parentHeap(node).get.value, node.value))
        assert(false)
    }
  }

  def decreaseKey(key: Key, value: Value): Unit = {
    index.get(key) match {
      case Some(node) =>
        if (node.value != value) {
          require(ord.lt(value, node.value))
          node.value = value
          upHeap(node)
        }
      case None => insert(key, value)
    }
  }

  def increaseKey(key: Key, value: Value): Unit = index.get(key) match {
    case Some(node) =>
      if (node.value != value) {
        require(ord.gt(value, node.value))
        node.value = value
        downHeap(node)
      }
    case None => insert(key, value)
  }

  def changeKey(key: Key, value: Value): Unit = index.get(key) match {
    case Some(node) if (node.value != value) => {
      node.value = value
      if (ord.gt(value, node.value))
        downHeap(node)
      else
        upHeap(node)
    }
    case None => insert(key, value)
  }

  def insert(key: Key, value: Value): Unit = {
    require(index.get(key).isEmpty)
    heap += key
    val node = new Node(value, heap.size - 1)
    index += (key -> node)
    upHeap(node)
  }

  def min: (Key, Value) = {
    if (heap.size <= 1)
      throw new NoSuchElementException
    val key = heap(rootIndex)
    (key, index(key).value)
  }

  def removeMin(): Unit = {
    // move min to last
    swapHeap(index(heap(rootIndex)), index(heap.last))

    // remove last
    index.remove(heap.last)
    heap.reduceToSize(heap.size - 1)

    // restore heap property
    if (nonEmpty)
      downHeap(index(heap(rootIndex)))
  }

  def dequeue(): (Key, Value) = {
    val res = min
    removeMin
    res
  }

  def apply(k: Key): Value = index(k).value
  def contains(k: Key): Boolean = index.contains(k)
  def get(k: Key): Option[Value] = index.get(k).map(_.value)
}

case class Path(path: Seq[Long], length_m: Double)

trait PathFinder {

  /**
    * @param from id of the source
    * @param to id of the destination
    * @return an optional map of predecessor ids if a path is found
    */
  def findPath(from: Long, to: Long): Option[Path]
}

object Solution extends App {
  val Array(nodes, edges, start, goal) = readLine.split(" ").map(_.toLong).toArray
  //val Array(nodes, edges, start, goal) =
  // "8 10 0 7".split(" ").map(_.toLong).toArray

  val dataForHeristic = readLine
                        .split(" ")
                        .map(_.toDouble)
                        .zipWithIndex
                        .map(item => item._2.toLong -> item._1)
                        .toMap

  def herustinc(input: Long): Double = {
    dataForHeristic(input)+(input.toDouble/10000)
  }

  val dataWithNodes = Array.fill(edges.toInt)(readLine).map(item => item.split(" ").map(_.toLong))

  val correctResult =
    Seq("0 15", "3 14", "1 16", "2 16", "5 15", "4 16", "6 16", "7 18").map(
      item => item.split(" ").map(numb => numb.toLong))

  val graph = (dataWithNodes.map(item => (item(1), item(0))) ++
               dataWithNodes.map(item => (item(0), item(1))))
    .groupBy(key => key._1)
    .mapValues(_.map(_._2).toVector)

  val cost = (dataWithNodes.map(item => (item(1), item(0), item(2))) ++
              dataWithNodes.map(item => (item(0), item(1), item(2))))
    .map(item => (item._1, item._2) -> item._3.toDouble)
    .toMap

  def expandNode(x: Long): Seq[Long] = {
    val res = graph.get(x).getOrElse(Seq.empty[Long])
    //System.out.println(s"expand ${x} ${res}")
    res
  }

  def costToNode(from: Long, to: Long): Double = {
    cost.get((from -> to)).getOrElse(0.0)
  }

  val pathFinder = new PathFinderAstar(expandNode, costToNode, herustinc)
  val steps = pathFinder.findPath(start, goal)

  val list = steps.get.path
  val intermidiate = list
                     .sliding(2)
                     .toList
                     .map(item => cost.get((item(0) -> item(1))).getOrElse(0.0))
  val result = list
               .zip((intermidiate).scanLeft(0.0)((acc, elem) => acc + elem))
               .map(item => (item._1, (item._2 + herustinc(item._1)).toInt))
  //result.foreach(item => println(item._1+" "+item._2))

}

object PathFinderAstar {
  def reconstructPath(from: Long,
                      to: Long,
                      parent: mutable.Map[Long, (Long, Double)]): Seq[Long] = {
    val reversed = mutable.ArrayBuffer[Long](to)
    var i = to
    do {
      i = parent.get(i).get._1
      reversed += i
    } while (i != from)
    reversed.reverse
  }
}

final class PathFinderAstar(
                             expand: (Long) => Seq[Long],
                             costFunction: (Long, Long) => Double,
                             heuristicCost: (Long) => Double
                           ) extends PathFinder {
  case class Stats(var maxFrontier: Int = 0)
  var stats = new Stats()

  def findPath(from: Long, to: Long): Option[Path] = {
    stats = new Stats()
    /// Predecessor of a given node with actual cost to get to
    val parent = mutable.OpenHashMap.empty[Long, (Long, Double)]

    // priority queue with (cost, id) ordered by the first element
    //val opendNodes =
    //  new mutable.PriorityQueue[(Double, Long)]()(Ordering.by(item => -item._1))

    val opendNodes = new IndexMinPQ[Long, Double](500)
    // visited nodes, these have been explored already, we won't expand these node nor add them to the frontier again
    val closedNodes = mutable.Set.empty[Long]

    // seed the frontier with the initial node
    //frontier += (0.0 -> from)
    opendNodes.insert(from, heuristicCost(from))
    //opendNodes.enqueue((heuristicCost(from), from))

    val memoazedNode = new mutable.HashMap[Long, Double]()
    while (opendNodes.nonEmpty) {
      val (stepFrom, fromCost) = opendNodes.dequeue()
      System.out.println(stepFrom + " " + fromCost.toInt)
      if (stepFrom == to) {
        return new Some(
          Path(PathFinderAstar.reconstructPath(from, to, parent), 0))
      }

      closedNodes += stepFrom
      expand(stepFrom).iterator.filter(!closedNodes.contains(_)).foreach {
        expandedNode =>
          val actualCost: Double =
            costFunction(stepFrom, expandedNode) + parent
                                                   .get(stepFrom)
                                                   .getOrElse(0, 0.0)
                                                   ._2

          //если уже содержится в опенед -> то только обновить веса, если нет, обновить веса и добавить в куе и в любом случае добавляем в мемед

          val fcost = actualCost + heuristicCost(expandedNode)
          val oldCost = memoazedNode.get(expandedNode)
          // shortest path found
          if (memoazedNode.contains(expandedNode)) {
            if (fcost < oldCost.getOrElse(Double.MaxValue)) {
              opendNodes.decreaseKey(expandedNode,  fcost)
              memoazedNode += (expandedNode -> fcost)
              parent(expandedNode)= (stepFrom, actualCost)
            }
          } else {
            opendNodes.insert(expandedNode,  fcost)
            memoazedNode += (expandedNode -> fcost)
            parent += (expandedNode -> (stepFrom, actualCost))
          }
      }

    }
    // no path found
    return None
  }

//
}

