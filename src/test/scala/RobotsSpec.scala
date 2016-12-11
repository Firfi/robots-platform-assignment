import collection.mutable.Stack
import org.scalatest._

class RobotsSpec extends FlatSpec with Matchers {


  "DroneCalculator" should "handle clean test data" in {
    DroneCalculator.moveDrones(Field(6, 6),
      List(
        Drone(Coords(1, 2), North)->List(TurnLeft, Move, TurnLeft, Move, TurnLeft, Move, TurnLeft, Move, Move),
        Drone(Coords(3, 3), East)->List(Move, Move, TurnRight, Move, Move, TurnRight, Move, TurnRight, TurnRight, Move)
      )
    ) should contain inOrderOnly (Drone(Coords(1, 3), North), Drone(Coords(5, 1), East))
  }

  "DroneCalculator" should "not move drones over each other" in {
    DroneCalculator.moveDrones(Field(6, 6),
      List(
        Drone(Coords(1, 3), North)->List(),
        Drone(Coords(1, 1), North)->List(Move, Move, Move, Move, Move, Move, Move, Move, Move, Move, Move, Move)
      )
    ) should contain inOrderOnly (Drone(Coords(1, 3), North), Drone(Coords(1, 2), North))
  }

}