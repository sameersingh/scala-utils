package org.sameersingh.utils.termutils

/**
 * @author sameer
 * @date 2/2/12
 */

object TextColor {
  val GREY = "\033[1;30m"
  val RED = "\033[1;31m"
  val YELLOW = "\033[1;33m"
  val PINK = "\033[1;34m"
  val MAGENTA = "\033[1;35m"
  val CYAN = "\033[1;36m"
  val WHITE = "\033[1;37m"
  val ENDC = "\033[0m"

  def color(str: String, col: String): String = "%s%s%s".format(col, str, ENDC)

  def error(str: String) = color(str, RED)

  def warn(str: String) = color(str, YELLOW)

  def emph(str: String) = color(str, CYAN)

  def main(args: Array[String]) {
    println("Hello " + color("World", GREY) + "!")
    println("Hello " + color("World", RED) + "!")
    println("Hello " + color("World", YELLOW) + "!")
    println("Hello " + color("World", PINK) + "!")
    println("Hello " + color("World", MAGENTA) + "!")
    println("Hello " + color("World", CYAN) + "!")
    println("Hello " + color("World", WHITE) + "!")
  }
}