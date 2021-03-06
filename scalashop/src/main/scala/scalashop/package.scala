
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }
//  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//    val pixels = {
//      for (
//        i <- -radius to radius;
//        j <- -radius to radius
//      ) yield (scalashop.clamp(x + i, 0, src.width - 1), scalashop.clamp(y + j, 0, src.height - 1))
//    }.distinct.map({
//      case (x, y) =>
//        val pixel = src(x, y)
//        (red(pixel), green(pixel), blue(pixel), alpha(pixel))
//    })
//
//    rgba(
//      pixels.map(_._1).sum / pixels.length,
//      pixels.map(_._2).sum / pixels.length,
//      pixels.map(_._3).sum / pixels.length,
//      pixels.map(_._4).sum / pixels.length
//    )
//  }
  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var i = x - radius
    var r, g, b, a = 0
    var count = 0
    while (i <= x + radius) {
      var j = y - radius
      while (j <= y + radius) {
        if (i == clamp(i, 0, src.width - 1) && j == clamp(j, 0, src.height - 1)) {
          r = r + red(src(i, j))
          g = g + green(src(i, j))
          b = b + blue(src(i, j))
          a = a + alpha(src(i, j))
          count = count + 1
        }
        j = j + 1
      }
      i = i + 1
    }
    r = r / count
    g = g / count
    b = b / count
    a = a / count
    rgba(clamp(r, 0, 255), clamp(g, 0, 255), clamp(b, 0, 255), clamp(a, 0, 255))
  }
}
