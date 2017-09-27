
import math._
import scala.util._

/**
  * Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine!
  * Created by Kotobotov.ru on 14.05.2017.
  *
  **/
object Player extends App {
  val projectcount = readInt
  for(i <- 0 until projectcount) {
    val Array(a, b, c, d, e) = for(i <- readLine split " ") yield i.toInt
  }

  // game loop
  while(true) {
    for(i <- 0 until 2) {
      val Array(target, _eta, _score, _storagea, _storageb, _storagec, _storaged, _storagee, _expertisea, _expertiseb, _expertisec, _expertised, _expertisee) = readLine split " "
      val eta = _eta.toInt
      val score = _score.toInt
      val storagea = _storagea.toInt
      val storageb = _storageb.toInt
      val storagec = _storagec.toInt
      val storaged = _storaged.toInt
      val storagee = _storagee.toInt
      val expertisea = _expertisea.toInt
      val expertiseb = _expertiseb.toInt
      val expertisec = _expertisec.toInt
      val expertised = _expertised.toInt
      val expertisee = _expertisee.toInt
    }
    val Array(availablea, availableb, availablec, availabled, availablee) = for(i <- readLine split " ") yield i.toInt
    val samplecount = readInt
    for(i <- 0 until samplecount) {
      val Array(_sampleid, _carriedby, _rank, expertisegain, _health, _costa, _costb, _costc, _costd, _coste) = readLine split " "
      val sampleid = _sampleid.toInt
      val carriedby = _carriedby.toInt
      val rank = _rank.toInt
      val health = _health.toInt
      val costa = _costa.toInt
      val costb = _costb.toInt
      val costc = _costc.toInt
      val costd = _costd.toInt
      val coste = _coste.toInt
      Console.err.println(s"$sampleid $carriedby $rank $expertisegain $health $costa $costb $costc $costd $coste")
    }

    // Write an action using println

    println("GOTO DIAGNOSIS")
    println("CONNECT 0")
    println("GOTO MOLECULES")
    println("CONNECT A")
    println("CONNECT A")
    println("CONNECT B")
    println("CONNECT C")
    println("CONNECT C")
    println("CONNECT D")
    println("GOTO LABORATORY")
    println("CONNECT 0")

  }
}
