package intersection

import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}

/**
 *     Object helpers with constants and functions for performing different
 *     tasks.
 *
 */

object helpers {

  /**
   *  First 4 two constant pairs are integers that change
   *  the position of car image in relation to the objects "real" location
   *  The position of the case class Car is changed to look like it is in the middle
   *  when these constants are reduced from place.x and place.y
   */

  val xDegree0 = 40
  val yDegree0 = 30

  val xDegree90 = 30
  val yDegree90 = 50

  val xDegree180 = 50
  val yDegree180 = 20

  val xDegree270 = 20
  val yDegree270 = 45


  /**
   *
   * @param img     -> The image, for example carImage
   * @param angle   -> How much it rotates. A 90 degree change
   *                   changes the image to face from East to North.
   */

  def rotateImg(img: BufferedImage, angle: Double) = {
    val rads = Math.toRadians(angle)
    val sin = Math.abs(Math.sin(rads))
    val cos = Math.abs(Math.cos(rads))
    val w = Math.floor(img.getWidth * cos + img.getHeight * sin).toInt
    val h = Math.floor(img.getHeight * cos + img.getWidth * sin).toInt
    val rotatedImage = new BufferedImage(w, h, img.getType)
    val at = new AffineTransform
    at.translate(w / 2, h / 2)
    at.rotate(rads, 0, 0)
    at.translate(-img.getWidth / 2, -img.getHeight / 2)
    val rotateOp = new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR)
    rotateOp.filter(img, rotatedImage)
  }
}

/**
 *    This Enumeration would be a constructor for the car.
 *
 *    Still needs to be looked into, since I have trouble
 *    putting for example "South" as a constructor for a Car.
 *
 */

object spawnDirection extends Enumeration {
    val South = Val(Vector2D(850, 997))
    val East = Val(Vector2D(1500, 370))
    val North = Val(Vector2D(650, -30))
    val West = Val(Vector2D(-30, 570))
  protected case class Val(spawn: Vector2D) extends super.Val

  }

