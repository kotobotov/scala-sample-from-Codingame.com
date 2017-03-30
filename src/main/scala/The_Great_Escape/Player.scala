

/**
  * https://www.codingame.com/ide/puzzle/great-escape
  * © kotobotov.ru
  * */
object Player extends App {
  // w: width of the board
  // h: height of the board
  // playercount: number of players (2 or 3)
  // myid: id of my player (0 = 1st player, 1 = 2nd player, ...)

  class Bot() {
    var x = 0
    var y = 0
    var wallsleft = 0


    def update(data1: Int, data2: Int, data3: Int) = {
      x = data1
      y = data2
      wallsleft = data3
    }
  }

  var bots = List[Bot]()

  class Cell(input: Int) {
    var revord = input
    var left = 0
    var right = 0
    var down = 0
    var up = 0

    def setLeft(input: Int): Unit = {
      left = input
    }

    def setRight(input: Int): Unit = {
      right = input
    }

    def setUp(input: Int): Unit = {
      up = input
    }

    def setDown(input: Int): Unit = {
      down = input
    }

    def getMax(): String = {
      if ((left > right) && (left > up) && (left > down)) "LEFT"
      else if ((right > left) && (right > up) && (right > down)) "RIGHT"
      else if ((down > right) && (down > up) && (down > left)) "DOWN" else "UP"
    }

    override
    def toString: String = {
      (s"\n    ($up)    \n") +
        (s"($left)  $revord  ($right) \n") +
        (s"  ($down)  \n")
    }
  }

  val Array(w, h, playercount, myid) = for (i <- readLine split " ") yield i.toInt
  var counter = 0

  for (i <- 0 until playercount) {
    bots ++= List[Bot](new Bot)
  }


  val data = Array.ofDim[Cell](w, h)
  for {
    i <- 0 until w
    j <- 0 until h
  } data(i)(j) = myid match {
    case 0 => new Cell((i * 100))
    case 1 => new Cell(1000 - (i * 100))
    case 2 => new Cell(j * 100)
  }

  def initRevords() = {
    for {
      i <- 0 until w
      j <- 0 until h
    } {
      data(i)(j).left = try {
        data(i - 1)(j).revord
      } catch {
        case e: Exception => 0
      }
      data(i)(j).right = try {
        data(i + 1)(j).revord
      } catch {
        case e: Exception => 0
      }
      data(i)(j).up = try {
        data(i)(j - 1).revord
      } catch {
        case e: Exception => 0
      }
      data(i)(j).down = try {
        data(i)(j + 1).revord
      } catch {
        case e: Exception => 0
      }
    }

  }

  initRevords()


  object Position {
    def getNextDirection(currentX: Int, currentY: Int) = {
      val otvet = data(currentX)(currentY).getMax
      println(otvet)
      otvet match {
        case "LEFT" => (currentX - 1, currentY)
        case "RIGHT" => (currentX + 1, currentY)
        case "DOWN" => (currentX, currentY + 1)
        case "UP" => (currentX, currentY - 1)
      }
    }
  }

  def putWall(x: Int, y: Int, direction: String) = {
    Console.err.println(s"wall to $x + $y + $direction")
    direction match {
      case "V" => data(x)(y).setLeft(0)
        data(x)(y + 1).setLeft(0)
        data(x - 1)(y).setRight(0)
        data(x - 1)(y + 1).setRight(0)
      case "H" => data(x)(y).setUp(0)
        data(x + 1)(y).setUp(0)
        data(x)(y - 1).setDown(0)
        data(x + 1)(y - 1).setDown(0)
    }
  }

  var CurrentPos = (0, 0)
  // game loop
  while (true) {
    for (i <- 0 until playercount) {
      // x: x-coordinate of the player
      // y: y-coordinate of the player
      // wallsleft: number of walls available for the player
      val Array(x, y, wallsleft) = for (i <- readLine split " ") yield i.toInt
      bots(i).update(x, y, wallsleft)
    }
    val wallcount = readInt // number of walls on the board
    for (i <- 0 until wallcount) {
      // wallx: x-coordinate of the wall
      // wally: y-coordinate of the wall
      // wallorientation: wall orientation ('H' or 'V')
      val Array(_wallx, _wally, wallorientation) = readLine split " "
      val wallx = _wallx.toInt
      val wally = _wally.toInt

      putWall(wallx, wally, wallorientation)
    }

    // Write an action using println

    Position.getNextDirection(bots(myid).x, bots(myid).y)
    // action: LEFT, RIGHT, UP, DOWN or "putX putY putOrientation" to place a wall

  }
}