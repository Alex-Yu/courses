package observatory


case class Location(lat: Double, lon: Double)


case class Color(red: Int, green: Int, blue: Int)

object Color {
  def black = Color(0, 0, 0)
  def white = Color(255, 255, 255)
}


