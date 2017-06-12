package observatory
import Math._
import com.sksamuel.scrimage.{Image, Pixel}

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
    def distance(l1: Location, l2: Location): Double =
      6371 * acos(sin(l1.lat) * sin(l2.lat) + cos(l1.lat) * cos(l2.lat) * cos(abs(l1.lon - l2.lon)))
    def omega(d: Double, p: Double): Double = 1 / pow(d, p)

    val tempByDist = temperatures.par.map { case (l, t) =>
      (distance(l, location), t)
    }

    val minD = tempByDist.minBy(_._1) //Could be optimized by aggregating minimum
    if (minD._1 <= 1) minD._2 else {
      val p = 2
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
    else {
      val clv = cl.toInt
      val crv = cr.toInt
      Color.fromInt(
        (clv + (value - l) * (crv - clv) / (r - l)).toInt
      )
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

}

