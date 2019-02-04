import org.apache.spark.{SparkConf, SparkContext}

package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1
  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("observatory")
  val sc: SparkContext = new SparkContext(conf)
  def fToC(f: Double): Double = (f - 32) * 5 / 9
}
