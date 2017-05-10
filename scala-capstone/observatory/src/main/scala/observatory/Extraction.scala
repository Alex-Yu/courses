package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.sql.{DataFrame, Row, SparkSession}

/**
  * 1st milestone: data extraction
  */


object Extraction {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Capstone")
      .config("spark.master", "local")
      .getOrCreate()

  import spark.implicits._

  private def toCelsius(fahrenheitT: Double): Double = (fahrenheitT - 32) / 1.8
  case class Station(stn: String, wban: String, lat: Double, lon: Double)
  case class TempByDay(stn: String, wban: String, month: Int, day: Int, celsiusT: Double)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    def fsPath(resource: String): String = Paths.get(getClass.getResource(resource).toURI).toString
//    def toOption[T](s: String, f: String => T): Option[T] = if (s.isEmpty) None else Some(f(s))

    val stationsDf: DataFrame = spark.sparkContext.textFile(fsPath(stationsFile))
      .map(_.split(","))
      .collect {
        case attr if attr.length >= 4 && attr.slice(2, 4).forall(_.nonEmpty) => Station(attr(0), attr(1), attr(2).trim.toDouble, attr(3).trim.toDouble)
      }.toDF()

    stationsDf.createOrReplaceTempView("stations")

    val tempByDayDF = spark.sparkContext.textFile(fsPath(temperaturesFile))
      .map(_.split(","))
      .collect {
        case attr if attr.length >= 5 && attr.slice(2, 5).forall(_.nonEmpty) && attr(4).trim != "9999.9" =>
          TempByDay(attr(0), attr(1), attr(2).trim.toInt, attr(3).trim.toInt, toCelsius(attr(4).trim.toDouble))
      }.toDF()

    tempByDayDF.createOrReplaceTempView("tempByDay")

    stationsDf.join(
      tempByDayDF, stationsDf.col("stn") === tempByDayDF.col("stn") && stationsDf.col("wban") === tempByDayDF.col("wban")
    ).select($"month", $"day", $"lat", $"lon", $"celsiusT")
      .collect
    .map(row => (
      LocalDate.of(year, row.getInt(0), row.getInt(1)),
      Location(row.getDouble(2), row.getDouble(3)),
      row.getDouble(4)
    ))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.par.groupBy(_._2)
      .mapValues { iter =>
        iter.aggregate(0d)((z, triple) => z + triple._3, _ + _) / iter.size
      }.seq
  }

}
