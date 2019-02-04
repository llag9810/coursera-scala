package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import org.apache.spark.rdd.RDD
import org.apache.log4j.{Level, Logger}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val radius = 6371.0

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val (sl, st) = temperatures.map { case (l, t) =>
      val dst = distance(l, location)
      if (dst < 1) return t
      val w = 1.0 / Math.pow(dst, 3.0)
      (w * t, w)
    }.unzip
    sl.sum / st.sum  }

  def sparkPredictTemperature(temperature: RDD[(Location, Temperature)], location: Location): Temperature = {
    val (num, denom, near) = temperature.aggregate((0.0, 0.0, Double.NaN))(
      (temp, pair) => {
        if (!temp._3.isNaN) (0.0, 0.0, temp._3)
        else if (distance(pair._1, location) <= 1) (0.0, 0.0, pair._2)
        else (temp._1 + math.pow(distance(location, pair._1), -3) * pair._2,
          temp._2 + math.pow(distance(location, pair._1), -3),
          Double.NaN)
      },
      (p1, p2) => (p1._1 + p2._1, p1._2 + p2._2, if (p1._3.isNaN) p2._3 else p1._3))
    if (!near.isNaN) near else num / denom
  }

  def distance(l1: Location, l2: Location): Double = {
    val angle =
      if (l1 == l2) 0.0
      else if (l1.lat == -l2.lat && Math.abs(l1.lat - l2.lat) == 180) math.Pi
      else math.acos(
        math.sin(math.toRadians(l1.lat)) * math.sin(math.toRadians(l2.lat))
          + math.cos(math.toRadians(l1.lat)) * math.cos(math.toRadians(l2.lat))
          * math.cos(math.toRadians(math.abs(l1.lon - l2.lon))))
    radius * angle
  }


  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val sortedPoints = points.toIndexedSeq.sorted(Ordering.by[(Temperature, Color), Temperature](_._1))
    if (value >= sortedPoints.last._1) sortedPoints.last._2
    else if (value <= sortedPoints.head._1) sortedPoints.head._2
    else {
      var i = 0
      while (sortedPoints(i + 1)._1 < value) {
        i += 1
      }

      def interpolationColor: (Double, Double) => Double = interpolation(value)(sortedPoints(i)._1, sortedPoints(i + 1)._1)

      val r = interpolationColor(sortedPoints(i)._2.red, sortedPoints(i + 1)._2.red)
      val g = interpolationColor(sortedPoints(i)._2.green, sortedPoints(i + 1)._2.green)
      val b = interpolationColor(sortedPoints(i)._2.blue, sortedPoints(i + 1)._2.blue)
      Color(Math.round(r).toInt, math.round(g).toInt, math.round(b).toInt)
    }
  }

  def interpolation(x: Double)(x1: Double, x2: Double)(y1: Double, y2: Double): Double = {
    y1 + (y2 - y1) / (x2 - x1) * (x - x1)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val array = new Array[Pixel](360 * 180)
    var i = 0
    while (i < array.length) {
      val x = i % 360
      val y = i / 360
      val temperature = predictTemperature(temperatures, Location(90 - y, x - 180))
      val color = interpolateColor(colors, temperature)
      array(i) = Pixel(color.red, color.green, color.blue, 255)
      i += 1
    }
    Image(360, 180, array)
  }

  def sparkVisualize(temperatures: RDD[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val array = new Array[Pixel](360 * 180)
    var i = 0
    while (i < array.length) {
      val x = i % 360
      val y = i / 360
      val temperature = sparkPredictTemperature(temperatures, Location(90 - y, x - 180))
      val color = interpolateColor(colors, temperature)
      array(i) = Pixel(color.red, color.green, color.blue, 255)
      i += 1
    }
    Image(360, 180, array)
  }
}

