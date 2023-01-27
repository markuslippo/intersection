package intersection

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

/**
 *
 *  A single TrafficLight stores four pictures of different states.
 *  TrafficLight has methods for drawing and changing the current state.
 *
 *
 * @param direction => the lane which this particular TrafficLight is
 *                     in charge of controlling. Direction "East" means
 *                     that this TrafficLight controls cars coming from the right.
 * @param pos       => A predestined Vector2D that tells where the TrafficLight
 *                     will be painted.
 */

case class TrafficLight(direction: String, val pos: Vector2D, var currentState: Int) {

  val pictures: Vector[BufferedImage] = Vector(
    ImageIO.read(new File("pictures/red.png")),           //state 0 is red
    ImageIO.read(new File("pictures/green.png")),         //state 1 is green
    ImageIO.read(new File("pictures/yellow.png")),        //state 2 is yellow
    ImageIO.read(new File("pictures/redyellow.png"))      //state 3 is red and yellow
          )

  def changeState(n: Int) = {
   currentState = n
  }

  def draw(g: Graphics2D, pictures: Vector[BufferedImage], pos: Vector2D, n: Int) = {
       g.drawImage(pictures(n), pos.x.toInt, pos.y.toInt, null)
  }

}

/**
 *  A TrafficLightsSystem takes four TrafficLights and has methods
 *  for controlling them.
 */

case class TrafficLightsSystem() {

    var horizontalGreenLight: Boolean = false
    var verticalGreenLight: Boolean = true

  /**
   *  A Vector[TrafficLight] containing the trafficLights meant for
   *  controlling horizontal lanes, as in "East" and "West" pointing lights.
   */

  val horizontal: Vector[TrafficLight] = Vector(
          new TrafficLight("East", new Vector2D(500, 175), 0),
          new TrafficLight("West", new Vector2D(950, 625), 0)
          )

   /**
   *  A Vector[TrafficLight] containing the trafficLights meant for
   *  controlling horizontal lanes, as in "East" and "West" pointing lights.
   */

  val vertical: Vector[TrafficLight] = Vector(
          new TrafficLight("South", new Vector2D(950, 175), 1),
          new TrafficLight("North", new Vector2D(500, 625), 1),
          )

  /**
   *    DrawLights function for drawing each Traffic Light individually
   */

  def drawLights(g: Graphics2D) = {
   horizontal.foreach(a => a.draw(g, a.pictures, a.pos, a.currentState))
   vertical.foreach(a => a.draw(g, a.pictures, a.pos, a.currentState))
  }

  /**
   *  Method for forwarding the LightSystemStep.
   */

  def lightSystemStep() = {

    var isChanging = false
    var timeOfChange = 0

    //Check whether its time to change lights
    if(SimulationApp.seconds % 15 == 0) {
      timeOfChange = SimulationApp.seconds
      isChanging = true
      horizontalGreenLight = !horizontalGreenLight
      verticalGreenLight = !verticalGreenLight
    }

    //Lights are changing
    if( isChanging && SimulationApp.seconds < timeOfChange + 3) {
        if(horizontalGreenLight) {
            horizontal.foreach(_.changeState(3))
            vertical.foreach(_.changeState(2))
        } else {
            horizontal.foreach(_.changeState(2))
            vertical.foreach(_.changeState(3))
        }
    } else {
      isChanging = false

      if(horizontalGreenLight) {
        horizontal.foreach(_.changeState(1))
        vertical.foreach(_.changeState(0))
      } else {
        horizontal.foreach(_.changeState(0))
        vertical.foreach(_.changeState(1))
      }

    }
  }
}
