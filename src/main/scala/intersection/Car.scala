package intersection

import helpers._

import java.awt.{Color, Graphics2D}
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

/**
 *  A case class Car that has methods for moving and drawing.
 *
 *
 * @param speed       -> A Vector2D for speed with x and y speed components.
 * @param place       -> A Vector2D for with x and y coordinates.
 * @param spawnPoint  -> A string for where the car spawned.
 * @param turn        -> Direction where car is headed, as in Which turn it takes.
 *
 */

 case class Car(var speed: Vector2D, place: Vector2D, spawnPoint: spawnDirection.Value, turn: String) {

  /**
   *   An area that is in front of the car, detecting other cars.
   *   Area is nonexistent when turning.
   */

  var areaDetector = new Area((0, 0), (0, 0))

  /**
   *   Headed is an Option[String] to help with knowing what direction the car is pointing.
   *   This is used in determing the areaDetector. Headed is None when car is in the middle
   *   of turning.
   */

   var headed: Option[String] = None
   spawnPoint match {
     case spawnDirection.South => headed = Some("North")
     case spawnDirection.North => headed = Some("South")
     case spawnDirection.East  => headed = Some("West")
     case spawnDirection.West  => headed = Some("East")
   }

  /**
   *   Variable for changing the extra angle when a car is turning.
   */

  var angle: Double = 0

   /**
    *  These are two Vectors, a slowing down vector and an accelerating vector.
    *  These will affect the speed, when needed.
    */

   val slowVector = Vector2D(0.03, 0.03)
   val accelerateVector = Vector2D(0.05, 0.05)
   var topSpeed = 1.8

   /**
    *  A constant for drawing the car image to look like
    *  position is in the middle of a car
    */

   val imagePosChangerX = {
    spawnPoint match {
    case spawnDirection.South => xDegree90
    case spawnDirection.West => xDegree0
    case spawnDirection.North => xDegree270
    case spawnDirection.East => xDegree180
    }
  }

    /**
    *  A constant for drawing the car image to look like
    *  position is in the middle of a car.
    */

    val imagePosChangerY = {
    spawnPoint match {
    case spawnDirection.South => yDegree90
    case spawnDirection.West => yDegree0
    case spawnDirection.North => yDegree270
    case spawnDirection.East => yDegree180
    }
  }

  /**
  *  Using the already implemented Random seed in Intersection
  *  to determine one of four Car colors.
  */
  val num = Intersection.randomSeed.nextInt(4)
  val carImage: BufferedImage = {
   num match {
     case 0 => ImageIO.read(new File("pictures/carImgYellow.png"))
     case 1 => ImageIO.read(new File("pictures/carImgDarkBlue.png"))
     case 2 => ImageIO.read(new File("pictures/carImgBlue.png"))
     case 3 => ImageIO.read(new File("pictures/carImgOrange.png"))
   }
  }

   def drawableImage(spawn: spawnDirection.Value, img: BufferedImage): BufferedImage = {
    spawn match {
      case spawnDirection.South => rotateImg(carImage, 270 + angle)
      case spawnDirection.West => rotateImg(img, angle)
      case spawnDirection.North => rotateImg(carImage, 90 + angle)
      case spawnDirection.East => rotateImg(carImage, 180 + angle)
    }
   }

   /**
    * Still a very raw version that moves the cars in a straight line.
    * Later this will be a long method, checking all possible obstacles
    * on whether car can move or not. It will include acceleration and
    * turning.
    */

   def move(hasToStop: Boolean) = {
        turn match {
          case "Left" =>
              goLeft(hasToStop)
          case "Right" =>
              goRight(hasToStop)
          case "Straight" =>
              goStraight(hasToStop)
            }
      }

    def goRight(hasToStop: Boolean) = {
      spawnPoint match {
        case spawnDirection.North => {
                                    if(place.y < 275) {
                                          goStraight(hasToStop)
                                    } else {
                                      if(place.y > 275 && place.y < 400){
                                      place.y = place.y + 0.8 * speed.y
                                      place.x = place.x - 0.8 * math.sqrt(speed.x)
                                      angle = angle + 0.6
                                      headed = None
                                      }
                                      else {
                                      angle = 90
                                      headed = Some("West")
                                      goStraight(false)
                                      }

                                  }
        }
        case spawnDirection.South => {
                                      if(place.y > 650) {
                                          goStraight(hasToStop)
                                    } else {
                                      if(place.y < 650 && place.y > 570){
                                      place.y = place.y - 0.8 * speed.y
                                      place.x = place.x + 0.8 * math.sqrt(speed.x)
                                      angle = angle - 358.7
                                      headed = None
                                      }
                                      else {
                                      angle = -270
                                      headed = Some("East")
                                      goStraight(false)
                                      }

                                  }
        }
        case spawnDirection.East => {
                                      if(place.x > 925) {
                                          goStraight(hasToStop)
                                    } else {
                                      if(place.x < 925 && place.x > 850){
                                      place.x = place.x - 0.8 * speed.x
                                      place.y = place.y - math.sqrt(speed.y)
                                      angle = angle + 1
                                      headed = None
                                      }
                                      else {
                                      angle = 90
                                      headed = Some("North")
                                      goStraight(false)
                                      }

                                  }
        }
        case spawnDirection.West => {
                                    if(place.x < 550) {
                                          goStraight(hasToStop)
                                    } else {
                                      if(place.y > 550 && place.y < 700){
                                      place.y = place.y + 0.8 * speed.y
                                      place.x = place.x + math.sqrt(speed.x)
                                      angle = angle + 1
                                      headed = None
                                      }
                                      else {
                                      angle = 90
                                      headed = Some("South")
                                      goStraight(false)
                                      }

                                  }
        }
      }
    }

    //if this is on then car can move
    var initiate = false
    def goLeft(hasToStop: Boolean) = {
      spawnPoint match {
        case spawnDirection.North =>
                                    if(place.y < 275) {
                                          goStraight(hasToStop)
                                    } else {
                                      if(place.y > 275 && place.y < 570){
                                      place.y = place.y + 1.5 * speed.y
                                      place.x = place.x + 0.8 * math.sqrt(speed.x)
                                      angle = angle - 0.6
                                      headed = None
                                      }
                                      else {
                                      angle = -90
                                      headed = Some("East")
                                      goStraight(false)
                                      }

                                    }
        case spawnDirection.South =>
                                     if(place.y > 650) {
                                          goStraight(hasToStop)
                                    } else {
                                      if(place.y < 650 && place.y > 400){
                                      place.y = place.y - 1.5 * speed.y
                                      place.x = place.x - 0.8 * math.sqrt(speed.x)
                                      angle = angle + 359
                                      headed = None
                                      }
                                      else {
                                      angle = 270
                                      headed = Some("West")
                                      goStraight(false)
                                      }

                                  }
        case spawnDirection.East  =>
                                    if(place.x > 925) {
                                          goStraight(hasToStop)
                                    } else {
                                      if(place.x < 925 && place.x > 700){
                                      place.x = place.x - 1.5 * speed.x
                                      place.y = place.y + 0.8 * math.sqrt(speed.y)
                                      angle = angle - 1
                                      headed = None
                                      }
                                      else {
                                      angle = -90
                                      headed = Some("South")
                                      goStraight(false)
                                      }
                                    }
        case spawnDirection.West  =>
                                    if(place.x < 550) {
                                          goStraight(hasToStop)
                                    } else {
                                      if(place.x > 550 && place.x < 850){
                                      place.x = place.x + 1.5 * speed.x
                                      place.y = place.y - 0.8 * math.sqrt(speed.y)
                                      angle = angle - 0.8
                                      headed = None
                                      }
                                      else {
                                      angle = 270
                                      headed = Some("North")
                                      goStraight(false)
                                      }

                                  }
      }
    }

    def goStraight(hasToStop: Boolean) = {
      headed match {
        case Some("South") => {
                                    if(isStationary) {
                                        if(hasToStop) {
                                        } else {
                                          this.accelerate()
                                          place.y = place.y + speed.y
                                        }
                                    } else if(hasToStop) {
                                        this.slowDown()
                                          place.y = place.y + speed.y
                                    } else if(speed.x < topSpeed || speed.y < topSpeed) {
                                          this.accelerate()
                                          place.y = place.y + speed.y
                                    } else {
                                          place.y = place.y + speed.y
                                    }
                                }
        case Some("North") => {
                                    if(isStationary) {
                                        if(hasToStop) {
                                        } else {
                                          this.accelerate()
                                          place.y = place.y - speed.y
                                        }
                                    } else if(hasToStop) {
                                        this.slowDown()
                                          place.y = place.y - speed.y
                                    } else if (speed.x < topSpeed || speed.y < topSpeed) {
                                          this.accelerate()
                                          place.y = place.y - speed.y
                                    } else {
                                          place.y = place.y - speed.y
                                    }
                                }
        case Some("West")  => {
                                    if(isStationary) {
                                        if(hasToStop) {
                                        } else {
                                          this.accelerate()
                                          place.x = place.x - speed.x
                                        }
                                    } else if(hasToStop) {
                                        this.slowDown()
                                          place.x = place.x - speed.x
                                    } else if(speed.x < topSpeed || speed.y < topSpeed) {
                                          accelerate()
                                          place.x = place.x - speed.x
                                    } else {
                                          place.x = place.x - speed.x
                                    }
                                }
        case Some("East")  => {
                                    if(isStationary) {
                                        if(hasToStop) {
                                        } else {
                                          this.accelerate()
                                          place.x = place.x + speed.x
                                        }
                                    }else if(hasToStop) {
                                        this.slowDown()
                                        place.x = place.x + speed.x
                                    }else if (speed.x < topSpeed || speed.y < topSpeed){
                                        this.accelerate()
                                        place.x = place.x + speed.x
                                    } else {
                                        place.x = place.x + speed.x
                                    }

                                  }
               }
    }

    def accelerate() = {
      speed = this.speed.accelerate(accelerateVector)
    }

    def slowDown() = {
      this.speed = speed.slow(slowVector)
    }

    def isStationary = {
      this.speed.x == 0 && this.speed.y == 0
    }

   def isInsideArea(area: Area): Boolean = {
      area.isInArea(this.place)
   }

    def drawArea(g: Graphics2D, area: Area) = {
    g.setColor(Color.blue)
    g.drawRect(areaDetector.xMin.toInt, areaDetector.yMin.toInt, (areaDetector.xMax - areaDetector.xMin).toInt, (areaDetector.yMax - areaDetector.yMin).toInt)
  }

   /**
    *  Method for drawing the car. The position is adjusted to look like the position
    *  is in middle of the car, since a picture's position is the top left corner.
    */

  def draw(g: Graphics2D) = {
    g.drawImage(drawableImage(spawnPoint, carImage), place.x.toInt - imagePosChangerX, place.y.toInt - imagePosChangerY, null)
    if(SimulationApp.techModeOn)
      drawArea(g, areaDetector)
  }

}


