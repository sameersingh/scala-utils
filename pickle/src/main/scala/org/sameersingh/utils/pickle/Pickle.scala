package org.sameersingh.utils.pickle

/**
 * @author sameer
 * @date 9/20/11
 */

trait Pickler[Value] {
  def saveToString(v: Value): String

  def loadFromString(s: String): Value
}

object IntPickler extends Pickler[Int] {
  def saveToString(v: Int) = v.toString

  def loadFromString(s: String) = s.toInt
}

object StringPickler extends Pickler[String] {
  def saveToString(v: String) = v

  def loadFromString(s: String) = s
}

object DoublePickler extends Pickler[Double] {
  def saveToString(v: Double) = v.toString

  def loadFromString(s: String) = s.toDouble
}
