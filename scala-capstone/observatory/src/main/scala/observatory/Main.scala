package observatory

import com.sksamuel.scrimage.{Image, Pixel}

object Main extends App {

  val res = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
  res take 20 foreach println

  val res2: Iterable[(Location, Double)] = Extraction.locationYearlyAverageRecords(res)

  res2 take 20 foreach println

  val colors = Seq(
    60d -> Color.white,
    32d -> Color(255, 0, 0),
    12d -> Color(255, 255, 0),
    0d -> Color(0, 255, 255),
    -15d -> Color(0, 0, 255),
    -27d -> Color(255, 0, 255),
    -50d -> Color(33, 0, 107),
    -60d -> Color(0, 0, 0)
  )

  val start = System.currentTimeMillis()
  val myImage = Visualization.visualize(res2, colors)
//  println(s"Visualization took ${(System.currentTimeMillis - start) / 1000}s") 65min
  myImage.output(new java.io.File("target/some-image.png"))
}
