package org.sameersingh.utils.pickle

import org.junit._
import Assert._
import Implicits._
import scala.collection.mutable.{HashMap, Map}

/**
 * @author sameer
 * @date 10/19/12
 */
@Test
class TutorialTest {

  @Test
  def testInt(): Unit = {
    val i = 10
    val str = pickle(i)
    println(str)
    val j = unpickle[Int](str)
    println(j)
    assertEquals(i, j)
  }

  @Test
  def testDouble(): Unit = {
    val i = 10.0
    val str = pickle(i)
    println(str)
    val j = unpickle[Double](str)
    println(j)
    assertEquals(i, j, 0.0001)
  }

  @Test
  def testString(): Unit = {
    val i = "Sameer"
    val str = pickle(i)
    println(str)
    val j = unpickle[String](str)
    println(j)
    assertEquals(i, j)
  }

  @Test
  def testPair(): Unit = {
    val i = Pair(10, "Sameer")
    val str = pickle(i)
    println(str)
    val j = unpickle(str, i)
    println(j)
    assertEquals(i, j)
  }

  @Test
  def testIntList(): Unit = {
    val i = List(1, 2, 3)
    val str = pickle(i)
    println(str)
    val j = unpickle(str, i)
    println(j)
  }

  @Test
  def testListOfIntLists(): Unit = {
    // TODO currently fails
    val i = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    val str = pickle(i)
    println(str)
    val j = unpickle(str, i)
    println(j)
  }

  @Test
  def testMap(): Unit = {
    val i: Map[Int, List[Int]] = new HashMap[Int, List[Int]]
    i ++= (0 until 10).map(i => Pair(i, (0 until i).toList)).toMap
    val str = pickle(i)
    println(str)
    val j = unpickle(str, i)
    println(j)
  }
  /*
 */

}
