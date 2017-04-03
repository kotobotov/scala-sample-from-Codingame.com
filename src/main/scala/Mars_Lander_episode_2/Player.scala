

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  def operator(a: (Int, Int), b: (Int, Int), op: (Int, Int) => Int): (Int, Int) = {
    (op(a._1, b._1), op(a._2, b._2))
  }

  def correcting(xDistance: Int, hspeed: Int, delta: Int): Int = {
    val predicted = math.toDegrees(math.atan(xDistance / targetLenght.toDouble))
    val diff = predicted + hspeed
    if (delta < 150) 0 else (diff * 0.8).toInt
  }

  def proporcional(x: Double, y: Double, targetx: Double, targety: Double) = {math.atan2(x - targetx, y - targety).toDegrees }


  var lastPoint = (0, 0)
  var target = lastPoint
  var targetLenght = 1000
  val area = for (i <- 0 until readInt) yield {
    val Array(landx, landy) = for (i <- readLine split " ") yield i.toInt
    if (lastPoint._2 == landy) target = ((lastPoint._1 + landx) / 2, landy)
    targetLenght = (landx - lastPoint._1)
    lastPoint = (landx, landy)
    (landx, landy)
  }

  def differential(x: Double, y: Double) = {
    val predictX = (x - lastPoint._1) / (y - lastPoint._2) * (target._2 - y)
    val predicAngle = proporcional(x, y, predictX, target._2)
    Console.err.println(s"predictX $predictX targetX ${target._1 } targetY ${target._2 } predicAngle $predicAngle")
    predicAngle
  }


  while (true) {
    val Array(x, y, hspeed, vspeed, fuel, rotate, power) = for (i <- readLine split " ") yield i.toInt

    val targetVec = operator((x, y), target, _ - _)
    val propAngle = proporcional(x.toDouble, y.toDouble, target._1.toDouble, target._2.toDouble)
    val angle = correcting(targetVec._1, hspeed, targetVec._2)
    val dif = differential(x.toDouble, y.toDouble)
    val corrected = propAngle - (propAngle - dif)* 0.8
    val thrust = if (vspeed < -35 || math.abs(hspeed) > 30) 4 else 3
    println(s"${corrected.toInt } $thrust")
  }

}