
/**
  * https://www.codingame.com/training/medium/shadows-of-the-knight-episode-1
  * Created by kotobotov.ru on 06.03.2017.
  */

object Player extends App {
  // w: width of the building.
  // h: height of the building.
  val Array(w, h) = for (i <- readLine split " ") yield i.toDouble
  val n = readInt
  // maximum number of turns before game over.
  val Array(startX, startY) = for (i <- readLine split " ") yield i.toDouble
  var left = 0.0
  var right = w - 1
  var up = 0.0
  var down = h - 1
  var centerX = startX
  var centerY = startY
  // game loop
  while (true) {
    val bombdir = readLine // the direction of the bombs from batman's current location (U, UR, R, DR, D, DL, L or UL)
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    def choser(input: String) = {
      input match {
        case "U" =>
          down = centerY
          centerY = math.floor((down + up) / 2)
        case "D" =>
          up = centerY
          centerY = math.ceil((down + up) / 2)
        case "L" =>
          right = centerX
          centerX = math.floor((left + right) / 2)
        case "R" =>
          left = centerX
          centerX = math.ceil((left + right) / 2)
      }
    }

    // Write an action using println
// To debug: Console.err.println("Debug messages...")
val data = bombdir.split("")
  .toList
  .foreach(item => choser(item))


    // the location of the next window Batman should jump to.
    println(s"${centerX.toInt } ${centerY.toInt }")
  }
}