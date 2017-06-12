package observatory

import java.awt.{Color => javaColor}

case class Location(lat: Double, lon: Double)


case class Color(red: Int, green: Int, blue: Int) {
  def toInt: Int = 256 * 256 * red + 256 * green + blue
 /* def toInt: Int = {
    val r = (red << 16) & 0x00FF0000
    val g = (green << 8) & 0x0000FF00
    val b = blue & 0x000000FF
    0xFF000000 | r | g | b
  }*/
}

object Color {
  def black = Color(0, 0, 0)
  def white = Color(255, 255, 255)
  def fromInt(v: Int): Color = Color(
    v / 256 / 256,
    v / 256 % (256 * 256),
    v % 256
  )
}


