package observatory
import Interaction._
import Visualization._

import scala.collection.mutable
/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val cache = mutable.HashMap[GridLocation, Temperature]()
    location => {
      if (cache.contains(location)) {
        cache(location)
      } else {
        val temperature = predictTemperature(temperatures, Location(location.lat, location.lon))
        cache += (location -> temperature)
        temperature
      }
    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val functions = temperaturess.map(makeGrid)
    val cache = mutable.HashMap[GridLocation, Temperature]()
    location => {
      if (cache.contains(location)) {
        cache(location)
      } else {
        val temperaturePair = functions.foldLeft((0.0, 0))((acc, func) => (acc._1 + func(location), acc._2 + 1))
        val temperature = temperaturePair._1 / temperaturePair._2
        cache += (location -> temperature)
        temperature
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    ???
  }


}

