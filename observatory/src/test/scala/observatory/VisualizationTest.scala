package observatory
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import java.lang.Math.abs
import observatory.Visualization.predictTemperature
trait VisualizationTest extends FunSuite with Checkers {
  test("predictTemperature: given location with same longitude then predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {
    val longitude = 10
    val locationX = Location(70.0, longitude)
    val locationY = Location(-89.0, longitude)

    val locationZCloserToX = Location(locationX.lat - 5, longitude)

    val locationZCloserToY = Location(locationY.lat + 5, longitude)

    val xTemperature = 10.0

    val yTemperature = 20.0

    val givenTemperatureByPositionPairs = Iterable(
      (locationY, yTemperature),
      (locationX, xTemperature)
    )

    val predictedTemperatureOfALocationCloseToX = predictTemperature(givenTemperatureByPositionPairs, locationZCloserToX)
    assert(abs(xTemperature - predictedTemperatureOfALocationCloseToX) < abs(yTemperature - predictedTemperatureOfALocationCloseToX))

    val actualTemperatureOfALocationCloseToY = predictTemperature(givenTemperatureByPositionPairs, locationZCloserToY)
    assert(abs(yTemperature - actualTemperatureOfALocationCloseToY) < abs(xTemperature - actualTemperatureOfALocationCloseToY))
  }

}
