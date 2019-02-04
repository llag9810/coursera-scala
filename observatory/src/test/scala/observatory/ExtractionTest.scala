package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait ExtractionTest extends FunSuite {

  lazy val testObject = Extraction
  test("read data test") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    testObject.locateTemperatures(1990, "/stations.csv", "/1990.csv")
  }
}