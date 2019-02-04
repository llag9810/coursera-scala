package observatory

import java.time.LocalDate

import org.apache.spark.rdd.RDD

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    sparkLocateTemperatures(year, stationsFile, temperaturesFile).collect.toSeq
  }

  def sparkLocateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): RDD[(LocalDate, Location, Temperature)] = {
    val stations = sc.textFile(getClass.getResource(stationsFile).toURI.toString).map(_.split(",", -1)).map(a => ((a(0), a(1)), (a(2), a(3))))
    val temperatures = sc.textFile(getClass.getResource(temperaturesFile).toURI.toString).map(_.split(",", -1)).map(a => ((a(0), a(1)), (a(2), a(3), a(4))))
    stations.filter(_._2 != Tuple2("", "")).join(temperatures).map {
      case ((_, _), ((latitude: String, longitude: String), (month: String, day: String, temperature: String))) =>
        (LocalDate.of(year, Integer.parseInt(month), Integer.parseInt(day)),
          Location(latitude.toDouble, longitude.toDouble),
          if (temperature == "") 9999.9 else fToC(temperature.toDouble)
        )
    }
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    sparkLocationYearlyAverageRecords(sc.parallelize(records.toSeq)).collect.toSeq
  }

  def sparkLocationYearlyAverageRecords(records: RDD[(LocalDate, Location, Temperature)]): RDD[(Location, Temperature)] = {
    records.map { case (_, location, temp) => (location, (temp, 1)) }
      .reduceByKey((a, b) => (a._1 + b._1, a._2 + b._2))
      .map { case (location, (sum, count)) => (location, sum / count) }
  }

}
