package intersection

import scala.collection.mutable.Buffer
import java.awt.Graphics2D
import scala.util.Random

object Intersection {

  var carsThroughIntersection = 0
  var carCrashHappened = false
  val randomSeed = new Random(System.currentTimeMillis())

  /**
   *   A Buffer for keeping track of the Cars in the simulation.
   *   For example, inRange method will be called on each element.
   */

  val vehicles = Buffer[Car]()

  /**
   *    A buffer for containing the carWreck that can possibly happen.
   *    Could also be changed to a Some() / None structure since
   *    there can only be a single carWreck, as the simulation ends
   *    when one happens.
   */

  val wreck = Buffer[CarWreck]()

  /**
   *
   *    TRAFFIC LIGHTS CONTROL
   *
   */


  /**
   * This is the TrafficLightsSystem that controls
   * all four TrafficLights.
   */

  val lightSystem = new TrafficLightsSystem()

  def lightsStep() = {
    lightSystem.lightSystemStep()
  }


  /**
   *
   *      CREATING AND REMOVING CARS
   *
   */



  val turns = Vector("Straight", "Right", "Left")

  /**
   *   Method for creating a car. When a button is pressed, create two random numbers.
   *   Use random numbers to get a spawnPoint and a turn. If a car can't spawn there,
   *   try to spawn the car to some other direction. If all spawns are occupied, don't do anything
   */

  val spawns = Vector(spawnDirection.North, spawnDirection.South, spawnDirection.East, spawnDirection.West)
  val spawnCoords = Vector((650, -30), (850, 997), (1500, 370), (-30, 570))

  def createRandomCar(): Option[Car] = {
    var car: Option[Car] = None

    var spawnNo = randomSeed.nextInt(4)
    var turnNo = randomSeed.nextInt(3)

    var done = false
    var triesSoFar = 0

    while (triesSoFar < 4 && !done) {
      val newCar =  new Car(Vector2D(1, 1), Vector2D(spawnCoords(spawnNo)._1, spawnCoords(spawnNo)._2), spawns(spawnNo), turns(turnNo))

      if(canSpawn(newCar)) {
        addCar(newCar)
        car = Some(newCar)
        done = true
      } else {

          if(spawnNo == 3)
              spawnNo = 0
          else
              spawnNo += 1
      }
      triesSoFar += 1
    }
    car
  }

  def createCar(spawn: spawnDirection.Value): Unit = {
    var turnNo = randomSeed.nextInt(3)

    val newCar = {
      spawn match {
      case spawnDirection.North => new Car(Vector2D(1, 1), Vector2D(spawnCoords(0)._1, spawnCoords(0)._2), spawnDirection.North, turns(turnNo))

      case spawnDirection.South => new Car(Vector2D(1, 1), Vector2D(spawnCoords(1)._1, spawnCoords(1)._2), spawnDirection.South, turns(turnNo))

      case spawnDirection.East  => new Car(Vector2D(1, 1), Vector2D(spawnCoords(2)._1, spawnCoords(2)._2), spawnDirection.East, turns(turnNo))

      case spawnDirection.West  => new Car(Vector2D(1, 1), Vector2D(spawnCoords(3)._1, spawnCoords(3)._2), spawnDirection.West, turns(turnNo))
      }
    }
    if(canSpawn(newCar))
        addCar(newCar)
  }


  def addCar(car: Car): Unit = {
    vehicles += car
    carsThroughIntersection += 1
  }

  def canSpawn(car: Car): Boolean = {
    var spawnable = true

    if(vehicles.isEmpty) {
        spawnable
    } else {

      car.spawnPoint match {
        case spawnDirection.North => for(oneCar <- vehicles)
                                    {
                                       if(oneCar.place.y < car.place.y + 100 && oneCar.place.x == car.place.x)
                                          spawnable = false
                                    }
        case spawnDirection.South => for(oneCar <- vehicles)
                                    {
                                       if(oneCar.place.y > car.place.y - 100 && oneCar.place.x == car.place.x)
                                          spawnable = false
                                    }
        case spawnDirection.East  => for(oneCar <- vehicles)
                                    {
                                       if(oneCar.place.x > car.place.x - 100 && oneCar.place.y == car.place.y)
                                          spawnable = false
                                    }
        case spawnDirection.West  => for(oneCar <- vehicles)
                                    {
                                       if(oneCar.place.x < car.place.x + 100 && oneCar.place.y == car.place.y)
                                          spawnable = false
                                    }
      }
    spawnable
    }
  }

  def removeCar(car: Car) = {
    vehicles -= car
  }


  /**
   *
   *      MOVEMENT
   *
   */


  /**
   *  The method step() is basically the main function of Intersection.
   *  It ties everything together, and is called from SimulationApp.
   *  Moves the cars, checks whether any cars are in range for a
   *  carWreck or not.
   */

  def step() = {
    if(vehicles.nonEmpty)
    {
      //An empty buffer for possible cars that are out of bounds
      val carsOutOfBounds: Buffer[Car] = Buffer()
      //Go through each car and see if a car wreck is possible
      wreckComponent()

        for(oneCar <- vehicles)
        {
          //If car is out of bounds, add it to carsOutOfBounds
          if(oneCar.place.x < - 90 || oneCar.place.x > 1550 )
          {
            carsOutOfBounds += oneCar
          }
          else if(oneCar.place.y < -90 || oneCar.place.y > 1050 )
          {
            carsOutOfBounds += oneCar        //car is not out of bounds, so check whether car has to stop or not
          } else
            {
                  var hasToStop = false
                  if(needsToAvoidOpposite(oneCar)) {
                      hasToStop = true
                  }
                  else if(lightIsRed(oneCar)) {
                      hasToStop = true
                  }
                  else if(carInFront((oneCar))) {
                      hasToStop = true
                  }
                  else if(carStillInMiddle(oneCar)) {
                      hasToStop = true
                  }

              oneCar.move(hasToStop)
            }
        }
        carsOutOfBounds.foreach(removeCar(_))
    }
  }

  val NorthArea = new Area((725, 575), (350, 100))
  val SouthArea = new Area((900, 750), (800, 675))
  val EastArea = new Area((1100, 925), (550, 250))
  val WestArea = new Area((550, 425), (750, 400))

  def lightIsRed(oneCar: Car): Boolean =  {
    var hasToStop = false
    oneCar.spawnPoint match {
      case spawnDirection.North => {
                 // val area = new Area((725, 575), (275, 150))
                  if(!lightSystem.verticalGreenLight && NorthArea.isInArea(oneCar.place))
                  hasToStop = true
                  }
      case spawnDirection.South => {
                 // val area = new Area((900, 750), (875, 775))
                  if(!lightSystem.verticalGreenLight && SouthArea.isInArea(oneCar.place))
                  hasToStop = true
                  }

      case spawnDirection.East => {
                   // val area = new Area((1050, 925), (475, 325))
                  if(!lightSystem.horizontalGreenLight && EastArea.isInArea(oneCar.place))
                  hasToStop = true
                  }

      case spawnDirection.West => {
                  // val area = new Area((550, 425), (650, 500))
                  if(!lightSystem.horizontalGreenLight && WestArea.isInArea(oneCar.place))
                  hasToStop = true
                  }
                }

    hasToStop
  }

  def carInFront(first: Car): Boolean = {
    var returnValue = false
    for(i <- vehicles) {
      if(first == i) {
      }else {
        first.headed match {
          case Some("North") =>
                     first.areaDetector = new Area( (first.place.x + 70, first.place.x - 70), (first.place.y, first.place.y - 200) )
                       if( first.areaDetector.isInArea(i.place) )
                          returnValue = true
          case Some("West") =>
                     first.areaDetector = new Area( (first.place.x, first.place.x - 200), (first.place.y + 70, first.place.y - 70) )
                       if( first.areaDetector.isInArea(i.place) )
                          returnValue = true
          case Some("East") =>
                     first.areaDetector = new Area((first.place.x + 200, first.place.x), (first.place.y + 70, first.place.y - 70))
                       if(first.areaDetector.isInArea(i.place) )
                          returnValue = true
          case Some("South") =>
                     first.areaDetector = new Area((first.place.x + 70, first.place.x - 70), (first.place.y + 200, first.place.y))
                       if(first.areaDetector.isInArea(i.place))
                          returnValue = true
          case None => first.areaDetector = new Area((0, 0), (0, 0))
         }
        }
      }
    returnValue
    }


  def carStillInMiddle(car: Car): Boolean = {
    val middleArea = new Area((925, 575), (650, 300))
    var returnValue = false

    for(oneCar <- vehicles) {
      if(car == oneCar) {
      } else {
       car.spawnPoint match {
         case spawnDirection.South => if(SouthArea.isInArea(car.place) && middleArea.isInArea(oneCar.place) && !oneCar.isStationary )
                                          if(oneCar.headed.isEmpty && oneCar.spawnPoint != car.spawnPoint)
                                              returnValue = true
                                          else if(oneCar.headed == Some("West"))
                                              returnValue = true
                                          else if(oneCar.headed == Some("East"))
                                              returnValue = true
                                          else
                                              returnValue = false
         case spawnDirection.North => if(NorthArea.isInArea(car.place) && middleArea.isInArea(oneCar.place) && !oneCar.isStationary )
                                         if(oneCar.headed.isEmpty && oneCar.spawnPoint != car.spawnPoint)
                                              returnValue = true
                                          else if(oneCar.headed == Some("West"))
                                              returnValue = true
                                          else if(oneCar.headed == Some("East"))
                                              returnValue = true
                                          else
                                              returnValue = false
         case spawnDirection.West  => if(WestArea.isInArea(car.place) && middleArea.isInArea(oneCar.place) && !oneCar.isStationary )
                                         if(oneCar.headed.isEmpty && oneCar.spawnPoint != car.spawnPoint)
                                              returnValue = true
                                          else if(oneCar.headed == Some("North"))
                                              returnValue = true
                                          else if(oneCar.headed == Some("South"))
                                              returnValue = true
                                          else
                                              returnValue = false
         case spawnDirection.East  => if(EastArea.isInArea(car.place) && middleArea.isInArea(oneCar.place) && !oneCar.isStationary )
                                         if(oneCar.headed.isEmpty && oneCar.spawnPoint != car.spawnPoint)
                                              returnValue = true
                                          else if(oneCar.headed == Some("South"))
                                              returnValue = true
                                          else if(oneCar.headed == Some("North"))
                                              returnValue = true
                                          else
                                              returnValue = false
       }
      }
    }
  returnValue
  }

  val NorthAreaPlus = new Area((725, 575), (400, 0))
  val SouthAreaPlus = new Area((900, 750), (967, 675))
  val EastAreaPlus = new Area((1471, 925), (550, 250))
  val WestAreaPlus = new Area((550, 0), (750, 400))

  def needsToAvoidOpposite(car: Car): Boolean = {
    var needsToWait = false

    if(car.turn == "Left")
      {
        if(car.initiate) {
          needsToWait = false
        } else {
       car.spawnPoint match {
         case spawnDirection.North =>

              if(lightSystem.verticalGreenLight && NorthArea.isInArea(car.place)) {

                    for(otherCar <- vehicles)
                      {
                       if(car == otherCar) {
                       } else {
                          if(SouthAreaPlus.isInArea(otherCar.place)) {
                              needsToWait = true

                              if(needsToAvoidOpposite(otherCar)) {
                                val num = randomSeed.nextInt(2)
                                if(num == 1)
                                    car.initiate = true
                                else
                                    otherCar.initiate = true
                                //NORTH SIDE GOES FIRST IN CASE BOTH CARS ARE TURNING LEFT
                              }

                          }
                       }
                      }

              } else {
                needsToWait = false
              }
         case spawnDirection.South =>
              if(lightSystem.verticalGreenLight && SouthArea.isInArea(car.place)) {

                    for(otherCar <- vehicles)
                      {
                       if(car == otherCar) {
                       } else {
                          if(NorthAreaPlus.isInArea(otherCar.place))
                              needsToWait = true
                       }
                      }

              } else {
                needsToWait = false
              }
         case spawnDirection.East  =>
               if(lightSystem.horizontalGreenLight && EastArea.isInArea(car.place)) {

                    for(otherCar <- vehicles)
                      {
                       if(car == otherCar) {
                       } else {
                          if(WestAreaPlus.isInArea(otherCar.place))
                              needsToWait = true
                       }
                      }

              } else {
                needsToWait = false
              }
         case spawnDirection.West  =>
               if(lightSystem.horizontalGreenLight && WestArea.isInArea(car.place)) {

                    for(otherCar <- vehicles)
                      {
                       if(car == otherCar) {
                       } else {
                          if(EastAreaPlus.isInArea(otherCar.place)) {
                              needsToWait = true


                         if(needsToAvoidOpposite(otherCar)) {
                                val num = randomSeed.nextInt(2)
                                if(num == 1)
                                    car.initiate = true
                                else
                                    otherCar.initiate = true
                              }
                         }
                       }
                      }
              } else {
                needsToWait = false
              }
      }
      }
    } else {
       needsToWait = false
    }

    needsToWait

  }

  /**
   *
   *      CAR WRECK
   *
   */

  /**
   *  wreckComponent. This is called on every step.
   *  If two cars are too close to each other, it throws
   *  a CarWreckException that prints the duration of the simulation.
   *
   */

  def wreckComponent()= {
    def checkForWreck(one: Car): Int = {
      var returnInt = -1
         for(i <- vehicles.indices) {
           if(inRange(one, vehicles(i))) {
              returnInt = i
           }
         }
      returnInt
      }

    try {
      for(oneCar <- vehicles) {
        var indexOfSecond = checkForWreck(oneCar)

        if(indexOfSecond == -1) {
        } else {
          wreck += CarWreck(oneCar.place, vehicles(indexOfSecond).place)
          vehicles.remove(indexOfSecond)
          vehicles-=oneCar
          throw CarWreckException("Simulation ended due to a car crash. More data of the simulation inside dataFromSimulation.txt!\n")
        }
      }
    } catch {
      case CarWreckException(text) =>
        carCrashHappened = true
        println(text)
        Writer.writeToFile("dataFromSimulation.txt", true)
    }
  }


  /**
   *   This method will is always called from step()
   *   as it checks cars' positions and returns true
   *   if two cars are too close to each other, then
   *   causing a carWreck.
   *
   * @param first    -> first car
   * @param second   -> second car
   *
   */

  def inRange(first: Car, second: Car): Boolean = {
     def YinRange(y: Double, y2: Double): Boolean = {
       var isInRange = false
       if(y < y2 + 75 && y > y2 - 75) {
         isInRange = true
       }
          isInRange
     }
    def XinRange(x: Double, x2: Double): Boolean = {
       var isInRange = false
       if(x > x2 - 75 && x < x2 + 75) {
         isInRange = true
       }
          isInRange
     }

    if(first == second)
        false
    else
        (YinRange(first.place.y, second.place.y) && XinRange(first.place.x, second.place.x))
  }


  /**
   *
   *      DRAW
   *
   */


  /**
   *  Method for drawing all the elements in the simulation.
   *  Draws the cars, the TrafficLightsSystem (4 TrafficLights)
   *  and a possible carWreck.
   */

  def draw(g: Graphics2D) = {
    vehicles.foreach(_.draw(g))
    wreck.foreach(_.draw(g))
    lightSystem.drawLights(g)
  }
}


