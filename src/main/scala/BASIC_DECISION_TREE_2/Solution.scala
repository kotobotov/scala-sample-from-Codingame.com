/**
 * Created by Kotobotov.ru on 05.02.2019.
 */

class Pupa(val i: Int, val s: Int, val f: Array[Int]) {}

object Solution extends App {

  def log2(value: Double): Double =
    Math.log(value) / Math.log(2)

  def binaryEntropy(pupae: Array[Pupa]): Double = {
    val count = pupae.size.toDouble
    pupae.groupBy(_.s).map { case (k, arr) =>
      val probability = arr.size / count
      - probability * log2(probability)
    }.sum
  }

  def weightedEntropy(pupae: Array[Pupa], pupa: Pupa, fi: Int): Double = {
    val count = pupae.size.toDouble
    val smallerPupae = pupae.filter(_.f(fi) < pupa.f(fi))
    val largerPupae = pupae.filter(_.f(fi) >= pupa.f(fi))
    smallerPupae.size / count * binaryEntropy(smallerPupae) +
    largerPupae.size / count * binaryEntropy(largerPupae)
  }

  class Group(val pupae: Array[Pupa], val f: Array[Int]) {
    var currentEntropyValue = binaryEntropy(pupae)
    var separator: Pupa = null
    var sfi: Int = 0
    for(i <- 0 until pupae.size; fi <- f) {
      val pupa = pupae(i)
      val weightedEntropyValue = weightedEntropy(pupae, pupa, fi)
      if(weightedEntropyValue < currentEntropyValue) {
        currentEntropyValue = weightedEntropyValue
        separator = pupa
        sfi = fi
      }
    }
    if(separator != null) {
      val count = pupae.size.toDouble
      val smallerPupae = pupae.filter(_.f(sfi) < separator.f(sfi))
      val largerPupae = pupae.filter(_.f(sfi) >= separator.f(sfi))
      val smallerGroup = new Group(smallerPupae, f)
      val largeGroup = new Group(largerPupae, f)
      currentEntropyValue =
        smallerPupae.size / count * smallerGroup.currentEntropyValue +
        largerPupae.size / count * largeGroup.currentEntropyValue
    }
  }

  val pn = readInt
  val fn = readInt
  val fm = readInt
  val initialPupae = (1 to pn).map { i =>
    val paras = readLine split " " map(_.toInt)
    new Pupa(paras(0), paras(1), paras.drop(2))
  }.toArray

  def search(f: Array[Int]): (Double, Array[Int]) = {
    if(f.size == fm) {
      val g = new Group(initialPupae, f)
      val entropy = g.currentEntropyValue
      Console.err.println(f.map(_ + 1).mkString(" ") + "   " + entropy)
      (entropy, f)
    } else {
      var entropy = Double.MaxValue
      var feature = Array[Int]()
      val fromF = if(f.size > 0) f.max + 1 else 0
      for(fi <- fromF until fn) {
        val nf = f :+ fi
        val (newEntropy, newFeatures) = search(nf)
        if(newEntropy < entropy) {
          entropy = newEntropy
          feature = newFeatures
        }
      }
      (entropy, feature)
    }
  }

  val (finalEntropy, finalFeature) = search(Array[Int]())

  println(finalFeature.map(_ + 1).mkString(" "))
}