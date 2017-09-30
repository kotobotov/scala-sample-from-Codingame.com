
sealed abstract class Cell

/**
  * https://www.codingame.com/ide/puzzle/great-escape
  * © kotobotov.ru
  * using Functional Reactive Programming
  * https://en.wikipedia.org/wiki/Functional_reactive_programming
  * */


import scala.util.DynamicVariable

class Signal[T](expr: => T) {
  import Signal._
  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  private var observed: List[Signal[_]] = Nil
  update(expr)

  protected def computeValue(): Unit = {
    for (sig <- observed)
      sig.observers -= this
    observed = Nil
    val newValue = caller.withValue(this)(myExpr())
    if (myValue != newValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  def apply() = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    caller.value.observed ::= this
    myValue
  }
}

class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}

object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}

object Signal {
  val caller = new DynamicVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T) = new Signal(expr)
}



object Player extends App{
  val moveCost = 1
  val minimalWeight = -99
  val initWeight = 99

  class Bot() {
    var x = 0
    var y = 0
    var wallsLeft = 0


    def nextDirection ={
      myWorld.grid(x, y).getNextDirection
    }

    def goto(direction:String) ={
      direction match {
        case "LEFT" => x-=1
        case "RIGHT" => x+=1
        case "DOWN" => y+=1
        case "UP" => y-=1
      }
    }
    def update(data1: Int, data2: Int, data3: Int) = {
      x = data1
      y = data2
      wallsLeft = data3
    }
  }
  class WorldGrid(x:Int, y:Int) {
    val data = Array.ofDim[Cell](high, wight)
    for {
      i <- 0 until high
      j <- 0 until wight
    } data(i)(j) = new Cell()

    for {
      x <- wight-1 to 0 by -1
      y <- high-1 to 0 by -1
    } {
      //we could use Option to work with edges, but i just use very small weight on direction to absent cells (wich is abroad our world) that allowd to avoid using Nil and processing that Nil
      // maybe i will rewrite code with Nil, just don't know how to use it correctly
      // we multiply to (-1) to get positive weight based on witch direction is main goal
      var d = 1
      var r = 1
      var l = 1

      myid match {
        case 0 => r = -1
        case 1 => l = -1
        case _ => d = -1
      }

      grid(x, y).down()= if(y < high - 1) grid(x, y + 1).getMax() - moveCost else minimalWeight * d
      grid(x, y).up()= if(y > 0) grid(x, y - 1).getMax() - moveCost else minimalWeight
      grid(x, y).left() = if(x > 0) grid(x - 1, y).getMax() - moveCost else minimalWeight * l
      grid(x, y).right()= if(x < wight - 1) grid(x + 1, y).getMax() - moveCost else minimalWeight * r
    }

    def putWall(x: Int, y: Int, direction: String)={
      Console.err.println(s"wall to ($x , $y) $direction")
      direction match {
        case "V" => grid(x, y).left() =0
          grid(x, y + 1).left() =0
          grid(x - 1, y).right() =0
          grid(x - 1, y + 1).right() =0
        case "H" => grid(x, y).up()=0
          grid(x, y - 1).down()=0
          grid(x+1, y).up() =0
          grid(x +1, y - 1).down()=0
      }
    }


    override def toString: String = data.map(item =>
      item.map(_.getMax()).mkString("|"))
                                    .mkString("\n")

    def grid(x:Int, y:Int)= data(y)(x)
  }
  object WorldGrid{
    def apply(x: Int, y: Int): WorldGrid = new WorldGrid(x, y)
  }
  class Cell {
    val left = Var(0)
    val right = Var(0)
    val down = Var(0)
    val up = Var(0)
    private val directions = Seq((down, "DOWN"), (up, "UP"), (left, "LEFT"), (right, "RIGHT"))
    val getMax = Var(directions.map(_._1()).max)
    def getNextDirection = directions.maxBy(_._1())._2
  }

  val Array(wight, high, playercount, myid) = for (i <- readLine split (" ")) yield i.toInt
  val myWorld = WorldGrid(wight,high)
  var bots = List[Bot]()

  for (i <- 0 until playercount) {
    bots ++= List[Bot](new Bot)
  }

  //myWorld.putWall(2, 2, "V")
  //myWorld.putWall(2, 4, "V")
  //myWorld.putWall(2, 5, "V")
  //bots(myid).goto("DOWN")
  //bots(myid).goto("DOWN")
  //bots(myid).goto("DOWN")
  //bots(myid).goto("DOWN")
  //println("init pos: "+ bots(myid).x + "  "+ bots(myid).y)




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
      // wallOrientation: wall orientation ('H' or 'V')
      val Array(_wallx, _wally, wallOrientation) = readLine split " "
      val wallx = _wallx.toInt
      val wally = _wally.toInt

      myWorld.putWall(wallx, wally, wallOrientation)
    }
    Console.err.println(myWorld.toString)
    println(bots(myid).nextDirection)
    // Write an action using println
    //there's no need to update state (because bot state updated every step, but i calculate next position any way, in case we just want to simulate with out external data)
    bots(myid).goto(bots(myid).nextDirection)
    // action: LEFT, RIGHT, UP, DOWN or "putX putY putOrientation" to place a wall

  }



}