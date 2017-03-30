// https://www.codingame.com/ide/puzzle/mars-lander-episode-1

object Player extends App {
  val surfacen = readInt // the number of points used to draw the surface of Mars.
  for (i <- 0 until surfacen) {
    val Array(landx, landy) = for (i <- readLine split " ") yield i.toInt
  }
  while (true) {
    val Array(x, y, hspeed, vspeed, fuel, rotate, power) = for (i <- readLine split " ") yield i.toInt
    if (vspeed < -39 && y < 3000) {println("0 4") } else println("0 0")
  }
}
