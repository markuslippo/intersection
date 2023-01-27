package intersection
import java.awt.Graphics2D
import java.io.File
import javax.imageio.ImageIO

/**
 *
 * @param place is a Vector2D with an immutable X and Y position.
 * Case class CarWreck is created in Intersection when two cars are hit.
 * This class only has one method of drawing the Image.
 *
 */

case class CarWreck(var place: Vector2D, var secondPlace: Vector2D) {

  val flame = ImageIO.read(new File("pictures/flame.png"))

  val X = (place.x + secondPlace.x)/2
  val Y = (place.y + secondPlace.y)/2

  def draw(g: Graphics2D) = {
   g.drawImage(flame, X.toInt - 45, Y.toInt - 45, null)
   g.drawImage(flame, X.toInt - 10, Y.toInt - 10, null)
   g.drawImage(flame, X.toInt - 80, Y.toInt - 10, null)
}
}