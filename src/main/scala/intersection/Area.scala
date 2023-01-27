package intersection

import java.awt.Graphics2D
import java.awt.Color

/**
 *
 * @param xTuple   Range of X: First Int is the upper limit, second is the lower limit
 * @param yTuple   Range of Y: First Int is the upper limit, second is the lower limit
 */

case class Area(xTuple: (Double, Double), yTuple: (Double, Double)) {

  var xMax = xTuple._1
  var xMin = xTuple._2
  var yMax = yTuple._1
  var yMin = yTuple._2

  def isInArea(place: Vector2D): Boolean = {
    var xInArea = false
    var yInArea = false

    if(place.x < xTuple._1 && place.x > xTuple._2) xInArea = true
    if(place.y < yTuple._1 && place.y > yTuple._2) yInArea = true

    if(xInArea && yInArea)
      true
    else
      false
  }

  def changeXMax(x: Double) = xMax = x
  def changeYMax(y: Double) = yMax = y
  def changeXMin(x: Double) = xMin = x
  def changeYMin(y: Double) = yMin = y

 def draw(g: Graphics2D) = {
    g.setColor(Color.blue)
    g.drawRect(xMin.toInt, yMin.toInt, (xMax - xMin).toInt, (yMax - yMin).toInt)
  }
}