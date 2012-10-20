package org.sameersingh.utils.termutils

/**
 * @author sameer
 * @date 10/19/12
 */
class ProgressBar(val total: Double, val prefix: String = "") {
  var _done = 0.0

  def update(done: Double) = {
    _done = done
    updateBar()
  }

  def updateBar() = {
    val percentage = (_done / total) * 100.0
    val percentLength = percentage.toString.length
    val str = new StringBuilder
    str.append(prefix + ": [")
    for (n <- 0 until 99) {
      if (n == 50) str.append(TextColor.warn(percentage.toString))
      if (n >= 50 && n < 50 + percentLength) {} // do nothing
      else {
        val char = if (n < percentage) TextColor.error("X") else "."
        str.append(char)
      }
    }
    str.append("]")
    if (percentage >= 99.999)
      str.append("\n")
    else
      str.append("\r")
    str.toString
  }
}

object ProgressBar {
  def main(args: Array[String]): Unit = {
    val pb = new ProgressBar(100.0, "testing")
    println("Starting...")
    var i = 0
    val n = 100
    while (i <= n) {
      print(pb.update(i))
      Thread.sleep(150)
      i += 1
    }
    println("Done...")
  }
}
