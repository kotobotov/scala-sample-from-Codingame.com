/**
 * Created by Dima Kubitskiy (@kotobotov) on 07.07.2019.
  * solution from Wie Chan
 */
class Pupa(val i: Int, val s: Int, val f: Array[Int]) {}
class DTree(val fTypes: Array[Boolean]) {
  def log2(value: Double): Double =
    Math.log(value) / Math.log(2)
  def binaryEntropy(pupae: Array[Pupa]): Double = {
    val count = pupae.size.toDouble
    pupae.groupBy(_.s).map { case (k, arr) =>
      val probability = arr.size / count
      - probability * log2(probability)
    }.sum
  }
  def dominate(pupae: Array[Pupa]): Int =
    pupae.groupBy(_.s).maxBy(_._2.size)._1
  def separate(pupae: Array[Pupa], separator: Pupa, fi: Int):
  (Array[Pupa], Array[Pupa]) =
    if(fTypes(fi)) pupae.partition(_.f(fi) == separator.f(fi))
    else pupae.partition(_.f(fi) >= separator.f(fi))
  def train(pupae: Array[Pupa], depth: Int): DNode = {
    var separator: Pupa = null
    var sfi = 0
    if(depth > 0) {
      var currentEntropyValue = binaryEntropy(pupae)
      val count = pupae.size.toDouble
      for(i <- 0 until pupae.size; fi <- 0 until fTypes.size) {
        val pupa = pupae(i)
        val (yesPupae, noPupae) = separate(pupae, pupa, fi)
        val weightedEntropyValue =
          yesPupae.size / count * binaryEntropy(yesPupae) +
            noPupae.size / count * binaryEntropy(noPupae)
        if(weightedEntropyValue < currentEntropyValue) {
          currentEntropyValue = weightedEntropyValue
          separator = pupa
          sfi = fi
        }
      }
    }
    if(separator == null) {
      val s = dominate(pupae)
      new DNode(s, null, 0, null, null)
    } else {
      val (yesPupae, noPupae) = separate(pupae, separator, sfi)
      new DNode(0, separator, sfi,
        train(yesPupae, depth - 1), train(noPupae, depth - 1))
    }
  }
  class DNode(val s: Int, val separator: Pupa, val fi: Int,
              val yesNode: DNode, val noNode: DNode) {
    def predict(pupae: Array[Pupa]): Array[(Int, Pupa)] = {
      if(s > 0) pupae.map(p => (s, p))
      else {
        val (yesPupae, noPupae) = separate(pupae, separator, fi)
        yesNode.predict(yesPupae) ++ noNode.predict(noPupae)
      }
    }
  }
}

object Solution extends App {
  def getAccuracy(result: Array[(Int, Pupa)]): Double = {
    val count = result.size.toDouble
    result.count(sp => sp._1 == sp._2.s) / count
  }
  val Array(fn, tpn, vpn, rpn) = readLine.split(" ").map(_.toInt)
  Console.err.println(s"Number of Features: $fn")
  Console.err.println(s"Number of Training Pupae: $tpn")
  Console.err.println(s"Number of Validation Pupae: $vpn")
  Console.err.println(s"Number of Real testing Pupae: $rpn")
  val fTypes = Array.fill(fn)({
    val Array(fname, ftype) = readLine.split(" ")
    ftype == "category"
  })
  Console.err.println("Features Loaded")
  val trainPupae = Array.fill(tpn)({
    val isf = readLine.split(" ").map(_.toInt)
    new Pupa(isf(0), isf(1), isf.drop(2))
  })
  Console.err.println("Training Set Loaded")
  val validPupae = Array.fill(vpn)({
    val isf = readLine.split(" ").map(_.toInt)
    new Pupa(isf(0), isf(1), isf.drop(2))
  })
  Console.err.println("Validation Set Loaded")
  val realPupae = Array.fill(rpn)({
    val isf = readLine.split(" ").map(_.toInt)
    new Pupa(isf(0), isf(1), isf.drop(2))
  })
  Console.err.println("Real Testing Set Loaded")

  val dTree = new DTree(fTypes)
  var check = true
  var trainAccuracy = 0.0
  var depth = 1
  var bestDT: dTree.DNode = null
  var bestAccuracy = 0.0
  var bestDepth = 1
  while(check) {
    val dNode = dTree.train(trainPupae, depth)
    val newTrainResult = dNode.predict(trainPupae)
    val newTrainAccuracy = getAccuracy(newTrainResult)
    val newValidResult = dNode.predict(validPupae)
    val newValidAccuracy = getAccuracy(newValidResult)
    if(newValidAccuracy > bestAccuracy) {
      bestDT = dNode
      bestAccuracy = newValidAccuracy
      bestDepth = depth
    }
    Console.err.println(s"Depth:$depth TrainAcc:$newTrainAccuracy ValidAcc:$newValidAccuracy")
    if(trainAccuracy == newTrainAccuracy) check = false
    else depth += 1
    trainAccuracy = newTrainAccuracy
  }
  val realResult = bestDT.predict(realPupae)
  val realAccuracy = getAccuracy(realResult)
  println(bestDepth)
  println((realAccuracy * 100 + 0.5).toInt)
}