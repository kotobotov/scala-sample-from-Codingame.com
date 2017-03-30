

/**
  * https://www.codingame.com/ide/puzzle/coders-strike-back
  * © kotobotov.ru
  */

object Player extends App {
  var firstTime = true
  var startDist = 0
  var sheldcounter = 0
  var counter = 0
  var stopPoint = 2800
  var boost = 0
  var lastMoove = 50.0
  var newMoove = 0.0
  var lastKoef = 0.0
  var predict = 0.0
  var enemySpeed = 0.0
  var lastEnemyX = 0.0
  var lastEnemyY = 0.0
  var lastX = 0.0
  var lastY = 0.0
  var correcterX = 0
  var correcterY = 0

  def corrector(speedX: Double, speedY: Double) = {
    (math.abs(speedX) > math.abs(speedY)) match {
      case true => correcterX = (math.abs(speedX) / speedX).toInt * 480
        correcterY = 0
      case false => correcterY = (math.abs(speedY) / speedY).toInt * 480
        correcterX = 0
    }
  }


  def predictor(x: Int, y: Int, opponentX: Int, opponentY: Int) = {
    corrector(x - lastX, y - lastY)
    lastX = x
    lastY = y

    newMoove = scala.math.sqrt(((x - opponentX) * (x - opponentX) + (y - opponentY) * (y - opponentY)))
    lastKoef = newMoove / lastMoove
    predict = newMoove * lastKoef
    enemySpeed = scala.math.sqrt(((lastEnemyX - opponentX) * (lastEnemyX - opponentX) + (lastEnemyY - opponentY) * (lastEnemyY - opponentY))) / 100
    Console.err.println("enemySpeed : " + enemySpeed)
    lastMoove = newMoove
    if (predict <= 820 && enemySpeed > 90) true else false
  }

  def reshalo(speed: String, frompredictor: Boolean) = {
    def thesame = as(speed)

    def as(input: String) = {
      sheldcounter += 1
      input
    }

    frompredictor match {
      case true => if (sheldcounter > 25) {
        sheldcounter = 0
        Console.err.println("SHHHHHHIEEEELD...")
        "SHIELD"
      } else thesame
      case false => thesame
    }
  }

  def getPower(ugol: Int, dist: Int): String = {
    counter += 1
    ugol match {
      //   case x if ((x>50 || x< -50)) => power = 95
      case x if (x > 100 || x < -100) => "0"
//    case x if (x > 60 || x < -60) => "85"
      case x if (x > 44 || x < -44) => "95"
      case x if (x > 10 || x < -10) => "100"
      case x if (x > 5 || x < -5) => "100"
      case _ => {
        Console.err.println("Edu rovno...")
        if (dist < stopPoint) "80"
        else {
          if (firstTime && counter > 80 && dist > 7000) {
            firstTime = false
            boost = 1
            "BOOST"
          } else "100"
        }
      }
    }
  }


  while (true) {
    // nextcheckpointx: x position of the next check point
    // nextcheckpointy: y position of the next check point
    // nextcheckpointdist: distance to the next checkpoint
    // nextcheckpointangle: angle between your pod orientation and the direction of the next checkpoint
    //   val Array(x, y, nextcheckpointx, nextcheckpointy, nextcheckpointdist, nextcheckpointangle) = for(i <- readLine split " ") yield i.toInt
    // val Array(opponentx, opponenty) = for(i <- readLine split " ") yield i.toInt
    //val predictConnect = predictor(x, y, opponentx, opponenty)
    println("0 0 0 0 0 0")
//        println( (nextcheckpointx - correcterX) +" " + (nextcheckpointy - correcterY) + " " + reshalo(getPower(nextcheckpointangle, nextcheckpointdist), predictConnect) )
  }

}