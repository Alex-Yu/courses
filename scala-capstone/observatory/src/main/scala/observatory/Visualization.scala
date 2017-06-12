package observatory
import net.jafama.FastMath._

import com.sksamuel.scrimage.{Image, Pixel, Color => SkColor}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    require(temperatures.nonEmpty, "Temperatures collection couldn't be empty")
    def distance(l1: Location, l2: Location): Double = {
      val l1LatRad = toRadians(l1.lat)
      val l2LatRad = toRadians(l2.lat)
      6371 * acos(sin(l1LatRad) * sin(l2LatRad) + cos(l1LatRad) * cos(l2LatRad) * cos(toRadians(abs(l1.lon - l2.lon))))
    }
      def omega(d: Double, p: Double): Double = 1 / pow(d, p)

//    val start = System.currentTimeMillis() // remove

    val tempByDist = temperatures.par.map { case (l, t) =>
      (distance(l, location), t)
    }

//    println(s"Distances calculation time = ${System.currentTimeMillis() - start}")

    val minD = tempByDist.minBy(_._1) //Could be optimized by aggregating minimum
    if (minD._1 <= 1) minD._2 else {
      val p = 3
      val (numerator, denominator) = tempByDist.aggregate((0d, 0d))(
        (z, dt) => {
          val (numerator, denominator) = z
          val (d, t) = dt
          val omegaI = omega(d, p)
          (numerator + omegaI * t, denominator + omegaI)
        },
        (l, r) => (l._1 + r._1, l._2 + r._2)
      )
      numerator / denominator
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    //TODO: Incorrect predicted color: Color(0,0,0). Expected: Color(255,0,0) (scale = List((0.0,Color(255,0,0)), (100.0,Color(0,0,255))), value = -10.0)
    val interval = points.foldLeft(((Double.MinValue, Color.black), (Double.MaxValue, Color.white))) {
      case(((l, cl), (r, cr)), (t, c)) =>
        if (t == value) ((t, c), (t, c))
        else if (t < value && t > l) ((t, c), (r, cr))
        else if (t > value && t < r) ((l, cl), (t, c))
        else ((l, cl), (r, cr))
    }

    val ((l, cl), (r, cr)) = interval
    if (l == r && cl == cr) cl
    else if (l == Double.MinValue) cl
    else if (r == Double.MaxValue) cr
    else Color(
      (cl.red + (value - l) * (cr.red - cl.red) / (r - l)).round.toInt,
      (cl.green + (value - l) * (cr.green - cl.green) / (r - l)).round.toInt,
      (cl.blue + (value - l) * (cr.blue - cl.blue) / (r - l)).round.toInt
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    def idxToLocation(idx: Int): Location = Location(90 - idx / 360, idx % 360 - 180)
    def toPixel(c: Color, alpha: Int = 255) = Pixel(c.red, c.green, c.blue, alpha)
    val pixels: Array[Pixel] = (0 until 360 * 180).toParArray.map { idx =>
      val l = idxToLocation(idx)
//      val start = System.currentTimeMillis()
      val t = predictTemperature(temperatures, l)
//      val tPredictTime = System.currentTimeMillis()
      val p = toPixel(interpolateColor(colors, t))
//      val cInterTime = System.currentTimeMillis()
//      println(s"idx=$idx, location from idx = $l, predicted temperature = $t, pixel = [r${p.red}g${p.green}b${p.blue}], " +
//        s"tempTime = ${tPredictTime - start}, colorTime = ${cInterTime - tPredictTime}")
      p
    }.toArray
    Image(360, 180, pixels)
  }

}

