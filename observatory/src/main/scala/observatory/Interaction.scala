package observatory


import com.sksamuel.scrimage.{Image, Pixel}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val lon = 360.0 * tile.x.toDouble / Math.pow(2, tile.zoom) - 180.0
    val lat = Math.atan(Math.sinh(Math.PI - tile.y * 2 * Math.PI / Math.pow(2, tile.zoom))) * 180 / Math.PI
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    import Visualization._
    val extraZoom = 8
    val size = Math.pow(2, extraZoom).toInt // 2 ^ 8
    val pixels = (0 until size * size).par
      .map(i => (i % size, i / size))
      .map { case (x, y) => Tile(tile.x * size + x, tile.y * size + y, tile.zoom + extraZoom) }
      .map(tileLocation)
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(color => Pixel(color.red, color.green, color.blue, 127))
      .toArray
    Image(size, size, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val tasks = for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      y <- 0 until Math.pow(2, zoom).toInt
      x <- 0 until Math.pow(2, zoom).toInt
    } yield Future {
      generateImage(year, Tile(x, y, zoom), data)
    }
    Await.result(Future.sequence(tasks), Duration.Inf)
  }
}
