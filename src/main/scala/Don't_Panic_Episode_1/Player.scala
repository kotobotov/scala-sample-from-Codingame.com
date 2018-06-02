/**
  * Created by Kotobotov.ru on 03.04.2017.
  * https://www.codingame.com/ide/puzzle/don't-panic-episode-1
  */
import math._
import scala.util._
object Player extends App {
  // nbfloors: number of floors
  // width: width of the area
  // nbrounds: maximum number of rounds
  // exitfloor: floor on which the exit is found
  // exitpos: position of the exit on its floor
  // nbtotalclones: number of generated clones
  // nbadditionalelevators: ignore (always zero)
  // nbelevators: number of elevators
  val Array(nbfloors, width, nbrounds, exitfloor, exitpos, nbtotalclones, nbadditionalelevators, nbelevators) = for(i <- readLine split " ") yield i.toInt
  val elevatorCoord = Array.fill(nbelevators)(0)
  for(i ← 0 until nbelevators) {
    // elevatorfloor: floor on which this elevator is found
    // elevatorpos: position of the elevator on its floor
    val Array(elevatorfloor, elevatorpos) = for(i <- readLine split " ") yield i.toInt
    elevatorCoord(elevatorfloor) = elevatorpos
    Console.err.println(s"$elevatorfloor , $elevatorpos")
  }
  case class BotState(floor:Int, pos:Int, direction:String)

  def isGoingToElevator(state:BotState):Boolean = {
    Try((if (state.direction=="LEFT") {state.pos - elevatorCoord(state.floor) }
         else {elevatorCoord(state.floor) - state.pos}) >= 0).getOrElse(true)
  }
  def legalMove(state:BotState):Boolean={
    var planingMove=0
    if (state.direction=="RIGHT") {planingMove = state.pos + 1}
    else {planingMove = state.pos-1}
    Console.err.println(s"$planingMove")
    planingMove match {
      case mypos:Int if ((mypos>width-1) || (mypos<0)) => false
      case _ => true
    }

  }
  def isRightDirection(state:BotState):Boolean={
    if (exitfloor == state.floor){
      (if (state.direction=="LEFT") {state.pos - exitpos }
       else {exitpos - state.pos}) >= 0
    } else true
  }
  var prevPos = 0
  // game loop
  while(true) {
    // clonefloor: floor of the leading clone
    // clonepos: position of the leading clone on its floor
    // direction: direction of the leading clone: LEFT or RIGHT
    val Array(_clonefloor, _clonepos, direction) = readLine split " "
    val clonefloor = _clonefloor.toInt
    val clonepos = _clonepos.toInt
    val currentBot = BotState(clonefloor, clonepos, direction)
    Console.err.println(s"$clonefloor $clonepos $direction")
    if (legalMove(currentBot) && isGoingToElevator(currentBot) && isRightDirection(currentBot)) println("WAIT") else
    prevPos = clonepos
  }
}
