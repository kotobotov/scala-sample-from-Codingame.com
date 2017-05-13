
/**
  * Created by Kotobotov.ru on 14.04.2017.
  */

object Player extends App {
  var myShipCount = 0 // the number of remaining ships
  var entityCount = 0 // the number of entities (e.g. ships, mines or cannonballs)
  val ENEMY = 0
  val ME = 1
  val high = 21
  val wight = 23
  var commands = List.empty[String]
  var ships = List.empty[Ship]
  var barrels = List.empty[Barrel]

  case class Ship(x: Int, y: Int, orientation: Int, speed: Int, energy: Int, owner: Int) {
    def nextMoove = {
      "WAIT"
      // убрать свой модификатор (отталкиватель), весов - затем операции выбора
      // проверить режим работы  - возможно это состояние меняется - атакующее, защищающее, востанавливает топливо
      // выбор максимального веса + доп эвристики? при выборе (возможно одинаковые веса)
    }
  }

  case class Barrel(x: Int, y: Int, energy: Int)

  case class Cannonball(targetX: Int, targetY: Int, from: Int, impact: Int)

  case class Mine(x: Int, y: Int)

  object Status extends Enumeration{
  val NOTVISITED, PLANTOVISIT, VISITED = values
  }

  class Cell(x: Int, y: Int) {
    var status = Status.NOTVISITED
    var coord = (x, y)
    var energy = 0.0

    def isEven = (y % 2) == 0

    def toVisit(x: Int, y: Int) = if (x >0 && y>0)WorldMap.grid(x)(y).status match {
      case Status.NOTVISITED => WorldMap.grid(x)(y).status = Status.PLANTOVISIT
        WorldMap.nodeToVisit = WorldMap.grid(x)(y) :: WorldMap.nodeToVisit
      case _ =>

    }

    def sendToNextVisit = {
      isEven match {
        case true =>                  toVisit(x, y+1); toVisit(x+1, y+1)
                    toVisit(x - 1, y);toVisit(x, y);   toVisit(x +1, y)
                                      toVisit(x, y-1); toVisit(x + 1, y - 1)
        case false =>toVisit(x - 1, y + 1);toVisit(x, y+1)
                     toVisit(x - 1, y);    toVisit(x, y);   toVisit(x +1, y)
                     toVisit(x - 1, y - 1);toVisit(x, y-1)
      }
    }

    override
    def toString = energy.toString
  }

  object Cell {
    def apply: Cell = new Cell(0, 0)
  }

  object Commander {
    var commands = List.empty[String]

    def add(command: String) = {commands = command :: commands }

    def print() = {commands.mkString(";") }

    def resetState() = {commands = List.empty[String] }
  }

  object WorldMap {
    val grid = Array.ofDim[Cell](wight, high) // transposed - first Y than X in coordinats
    def render() = {
      grid.foreach(item => Console.err.println(item.mkString(" | ")))
    }

    var nodeToVisit = List.empty[Cell]

    def add[T](obj: List[T]) = {
      obj.foreach {
        case item: Barrel => grid(item.x)(item.y).energy = item.energy.toDouble
        case _ =>
      }
    }

    def add[T](obj: T) = {}

    def calculate() = {}

    def predict(step: Int) = {}
  }

  def readGameData() = {
    ships = List.empty[Ship]
    barrels = List.empty[Barrel]
    myShipCount = readInt
    entityCount = readInt
    for (i <- 0 until entityCount) {
      val Array(_entityid, entitytype, _x, _y, _arg1, _arg2, _arg3, _arg4) = readLine split " "
      entitytype match {
        case "SHIP" => ships = Ship(_x.toInt, _y.toInt, _arg1.toInt, _arg2.toInt, _arg3.toInt, _arg4.toInt) :: ships
        case "BARREL" => barrels = Barrel(_x.toInt, _y.toInt, _arg1.toInt) :: barrels
        //при раставлении весов, учитывать расположение короткое до противника или меня -кто к противнику ближе, не ставить веса
        case "CANNONBALL" => WorldMap.add(Cannonball(_x.toInt, _y.toInt, _arg1.toInt, _arg2.toInt))
        case "MINE" => WorldMap.add(Mine(_x.toInt, _y.toInt))
        case _ => Console.err.println("INPUT DATA ERROR")
      }
    }
    WorldMap.add(barrels)
    WorldMap.add(ships)
  }

  def calculateMap() = {}

  // game loop
  while (true) {
    readGameData()
    Commander.resetState()
    WorldMap.calculate()
    WorldMap.render()
    ships.filter(_.owner == ME)
      .foreach(ship => Commander.add(ship.nextMoove))
    Commander.print()
  }
}
