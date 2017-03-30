// https://www.codingame.com/ide/puzzle/onboarding

object Player extends App {

  // game loop
  while(true) {
    val enemy1 = readLine // name of enemy 1
    val dist1 = readInt // distance to enemy 1
    val enemy2 = readLine // name of enemy 2
    val dist2 = readInt // distance to enemy 2
    println(if (dist1 < dist2) (enemy1) else (enemy2))
  }
}