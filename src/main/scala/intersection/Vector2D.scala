package intersection

case class Vector2D(var x: Double, var y: Double) {


  /**
   * Add two vectors and return their sum vector
   */
  def + (other: Vector2D) = {
    Vector2D(x + other.x, y + other.y)
  }

  /**
   * Accelerate function that either accelerates or slows down.
   * This affects the speed Vector.
   *
   * @param accelerator  -> each car has an acceleration value.
   */

  def accelerate(accelerator: Vector2D): Vector2D = this + accelerator

  def slow(accelerator: Vector2D): Vector2D = {
    var returnX: Double = 0
    var returnY: Double = 0

      if(this.x - accelerator.x < 0) {
      } else {
        returnX = this.x - accelerator.x
      }

      if(this.y - accelerator.y < 0) {
      } else {
        returnY = this.y - accelerator.y
      }
    Vector2D(returnX, returnY)
  }



}
