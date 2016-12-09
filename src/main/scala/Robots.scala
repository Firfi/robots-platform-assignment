import scala.annotation.tailrec
import scala.io.StdIn.readLine

sealed trait Direction extends Product with Serializable {
  override def toString = Direction.d2l(this)
}
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

object Direction {
  val letterToDirection: Map[String, Direction] = Map("N" -> North, "S" -> South, "E" -> East, "W" -> West)
  val directionToLetter: Map[Direction, String] = letterToDirection.toList.map(_.swap).toMap
  def l2d(s: String): Option[Direction] = letterToDirection.get(s)
  def d2l(d: Direction): String = directionToLetter(d)
}

sealed trait Action extends Product with Serializable
case object TurnLeft extends Action
case object TurnRight extends Action
case object Move extends Action

object Action {
  val letterToAction: Map[String, Action] = Map("L" -> TurnLeft, "R" -> TurnRight, "M" -> Move)
  val actionToLetter: Map[Action, String] = letterToAction.toList.map(_.swap).toMap
  def l2a(s: String): Option[Action] = letterToAction.get(s)
  def a2l(a: Action): String = actionToLetter(a)
}

case class Coords(x: Int, y: Int)

case class Field(xSize: Int, ySize: Int) // assuming int is enough
case class Drone(coords: Coords, direction: Direction) {
  override def toString = s"${coords.x} ${coords.y} $direction"
}

object Alias {
  type Robot = (Drone, Seq[Action]) // drone with program loaded
}

object DroneCalculator {

  // by a clock hand
  val directions = Array(North, East, South, West)
  val directionIndexes = directions.zipWithIndex.toMap

  def moveCoords(coords: Coords, d: Direction, field: Field): Coords = {
    val newCoords = d match {
      case North => coords.copy(y = coords.y + 1)
      case East => coords.copy(x = coords.x + 1)
      case South => coords.copy(y = coords.y - 1)
      case West => coords.copy(x = coords.x - 1)
    }
    if (newCoords.x >= field.xSize || newCoords.y >= field.ySize || newCoords.x < 0 || newCoords.y < 0) {
      println(s"wrong coords ${newCoords.x} ${newCoords.y}")
      // ignore wrong coords
      coords
    } else newCoords
  }

  def moveDrone(field: Field, d: Drone, a: Action): Drone = {
    val index = directionIndexes(d.direction)
    def turn(where: Int) = directions(Math.floorMod(index + where, directions.length))
    a match {
      case TurnLeft => d.copy(direction = turn(-1))
      case TurnRight => d.copy(direction = turn(1))
      case Move => d.copy(coords = moveCoords(d.coords, d.direction, field))
    }
  }
  @tailrec
  def moveDrone(field: Field, d: Drone, actions: Seq[Action]): Drone = {
    actions.toList match {
      case Nil => d
      case a :: nextActions => moveDrone(field, moveDrone(field, d, a), nextActions)
    }
  }
  def moveDrones(field: Field, robots: Seq[Alias.Robot]): Seq[Drone] = {
    robots.map { case (d: Drone, actions: Seq[Action]) => moveDrone(field, d, actions) }
  }
}

object Robots {
  def main(args: Array[String]) = {
    val drones = DroneCalculator.moveDrones(field, robots)
    drones.foreach(println)
  }
  lazy val field: Field = readLine.split(" ").toSeq.map(_.toInt) match {
    case Seq(rightX: Int, topY: Int, _*) => Field(rightX + 1, topY + 1)
  }
  lazy val robots: Seq[Alias.Robot] = {
    def _read(acc: Seq[Alias.Robot]): Seq[Alias.Robot] = {
      readRobot match {
        case Some(robot) => _read(robot +: acc)
        case None => acc
      }
    }
    _read(List.empty).reverse
  }
  def readRobot: Option[Alias.Robot] = (readLine.split(" ") match {
    case Array(xS: String, yS: String, directionS: String) =>
      Direction.l2d(directionS).map {d => Drone(Coords(xS.toInt, yS.toInt), d)} // TODO wrong initial coords?
    case _ => None
  }).map { drone => (drone, readLine.split("").toSeq.flatMap(Action.l2a)) }
}
