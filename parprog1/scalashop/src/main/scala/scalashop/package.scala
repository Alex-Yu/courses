
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    def getClampedData(x: Int, y: Int): (Int, Int) = {
      val xClamped = clamp(x, 0, src.width - 1)
      val yClamped = clamp(y, 0, src.height - 1)
      (xClamped, yClamped)
    }

    var dy = -radius
    var dx = -radius
    val xyClamped = collection.mutable.Set.empty[(Int, Int)]

    while (dy <= radius) {
      while (dx <= radius) {
        xyClamped += getClampedData(x + dx, y + dy)
        dx += 1
      }
      dy += 1
      dx = -radius
    }


    val t = xyClamped.map { p =>
      val rgba = src(p._1, p._2)
      (red(rgba), green(rgba), blue(rgba), alpha(rgba))
    }.foldLeft((0, 0, 0, 0)) {
      (z, e) => (z._1 + e._1, z._2 + e._2, z._3 + e._3, z._4 + e._4)
    }

    val s = xyClamped.size
    rgba(
      t._1 / s,
      t._2 / s,
      t._3 / s,
      t._4 / s
    )
  }

}
