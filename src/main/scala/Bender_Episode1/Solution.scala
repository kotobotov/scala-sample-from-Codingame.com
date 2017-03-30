
/**
  * https://www.codingame.com/training/medium/bender-episode-1
  * Created by kotobotov.ru on 28.03.2017.
  */

object Solution extends App {
  val Array(height, c) = for (i <- readLine split " ") yield i.toInt
  var commands = List.empty[String]
  var data = for (i <- 0 until height) yield {
    val row = readLine
    row.indexOf("@") match {
      case -1 =>
      case item: Int => Bender.setLocation(item, i)
        Bender.setNext("SOUTH")
    }
    row.indexOf("T") match {
      case -1 =>
      case item: Int => if (Bender.teleport1._1 == -1) Bender.teleport1 = (item, i)
      else Bender.teleport2 = (item, i)
    }

    Bender.maxTurns = height * c
    Console.err.println(row.toList.mkString(""))
    row.replace("@", " ").split("")
  }

  object Bender {
    var maxTurns = -1
    var teleport1 = (-1, -1)
    var teleport2 = (-1, -1)
    var state = "SOUTH"
    var changeStateCounter = 0
    var reversed = false
    var drunk = false
    var currentX = 0
    var currentY = 0
    var nextX = 0
    var nextY = 0
    var turnsCounter = 0

    def setLocation(x: Int, y: Int) = {
      currentX = x
      currentY = y
    }


    def currentLocation = (currentX, currentY)

    def nextLocation = (nextX, nextY)

    def reactTo(frontBloc: String) = {
      frontBloc match {
        case "$" => goTo(state)
          state = "die"
        case "#" => changeState(changeStateCounter)
          setNext(state)
        case "X" => drunk match {
          case false => changeState(changeStateCounter)
          case true => goTo(state)
        }
          setNext(state)
        case _ => goTo(state)
          setNext(state)
      }
    }


    def changeState(currentTry: Int) = {
      state = reversed match {
        case true =>
          currentTry match {
            case 0 => "WEST"
            case 1 => "NORTH"
            case 2 => "EAST"
            case 3 => "SOUTH"
          }
        case false =>
          currentTry match {
            case 0 => "SOUTH"
            case 1 => "EAST"
            case 2 => "NORTH"
            case 3 => "WEST"
          }
      }
      changeStateCounter = currentTry + 1
    }


    def goTo(direction: String) = {
      changeStateCounter = 0
      turnsCounter += 1
      commands ++= List(direction)
      direction match {
        case "SOUTH" => currentY += 1
        case "EAST" => currentX += 1
        case "NORTH" => currentY -= 1
        case "WEST" => currentX -= 1
      }
      data(Bender.currentY)(Bender.currentX) match {
        case "$" => state = "die"
        case "I" => reversed = !reversed
        case "B" => drunk = !drunk
        case "S" => state = "SOUTH"
        case "E" => state = "EAST"
        case "N" => state = "NORTH"
        case "W" => state = "WEST"
        case "T" => teleport(currentX, currentY)
        case "X" => data(Bender.currentY)(Bender.currentX) = " "
        case " " =>
        case _ =>
      }
      Bender.render()
    }

    def teleport(x: Int, y: Int) = {
      if (x == teleport1._1 && y == teleport1._2)
        setLocation(teleport2._1, teleport2._2) else
        setLocation(teleport1._1, teleport1._2)
    }

    def setNext(direction: String) = {
      direction match {
        case "SOUTH" => Bender.nextY = currentY + 1
          Bender.nextX = currentX
        case "EAST" => Bender.nextX = currentX + 1
          Bender.nextY = currentY
        case "NORTH" => Bender.nextY = currentY - 1
          Bender.nextX = currentX
        case "WEST" => Bender.nextX = currentX - 1
          Bender.nextY = currentY
      }
    }

    def render() = {
      val temp = data(currentY)(currentX)
      data(currentY)(currentX) = "@"
      for (i <- 0 until height) {
        Console.err.println(data(i).toList.mkString(""))
      }
      data(currentY)(currentX) = temp
    }
  }

  while (Bender.state != "die" && Bender.turnsCounter < Bender.maxTurns) {
    Bender.reactTo(data(Bender.nextY)(Bender.nextX))
  }

  Bender.state match {
    case "die" => commands.foreach(println(_))
    case _ => println("LOOP")
  }

}
