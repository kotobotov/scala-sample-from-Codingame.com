

/** https://www.codingame.com/ide/puzzle/the-descent
  * The while loop represents the game.
  * Each iteration represents a turn of the game
  * where you are given inputs (the heights of the mountains)
  * and where you have to print an output (the index of the mountain to fire on)
  * The inputs you are given are automatically updated according to your last actions.
  **/
object Player extends App {

  // game loop
  while (true) {
    var max = 0
    var shot_index = 0
    for (i <- 0 until 8) {
      val mountainh = readInt // represents the height of one mountain.
      if (max < mountainh) {
        max = mountainh
        shot_index = i
      }
    }
    println(shot_index) // The index of the mountain to fire on.
  }
}