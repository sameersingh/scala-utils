package org.sameersingh.utils.misc

import org.sameersingh.utils.timing.TimeUtil
import collection.mutable.{Map, HashMap, Set, HashSet}

/**
 * User: sameer
 * Date: May 6, 2010
 */

trait PairMap[V] {
  def put(i: Int, j: Int, v: V): V

  def get(i: Int, j: Int): Option[V]

  def getOrElse(i: Int, j: Int, v: () => V): V = {
    val result: Option[V] = get(i, j)
    if (result.isDefined) result.get else v()
  }

  def getOrElseUpdate(i: Int, j: Int, v: () => V): V = {
    val result: Option[V] = get(i, j)
    if (!result.isDefined) put(i, j, v()) else result.get
  }

  def clear: Unit

  def size: Int

  def keys: Iterable[Int]
}

class SparsePairMap[V](val symmetric: Boolean) extends PairMap[V] {
  val _keys: Set[Int] = new HashSet
  val _map: Map[Int, Map[Int, V]] = new HashMap

  def this() = this (true)

  def put(i: Int, j: Int, v: V): V = {
    _keys += i
    _keys += j
    val (a: Int, b: Int) = get_keys(i, j)
    _map.getOrElseUpdate(a, new HashMap)(b) = v
    /*val subMap: Map[Int, V] = if (_map.contains(a)) _map(a)
    else {val m: Map[Int, V] = new HashMap; _map(a) = m; m}
    subMap(b) = v*/
    v
  }

  def get(i: Int, j: Int): Option[V] = {
    val (a: Int, b: Int) = get_keys(i, j)
    for (subMap <- _map.get(a)) {
      for (value: V <- subMap.get(b))
        return Some(value)
    }
    /*if (_map.contains(a)) {
      val subMap = _map(a)
      if (subMap.contains(b))
        new Some(subMap(b))
      else None
    }*/
    None
  }

  def clear = _map.clear

  private def get_keys(i: Int, j: Int): (Int, Int) = {
    if (symmetric)
      if (i <= j) (i, j) else (j, i)
    else (i, j)
  }

  override def toString = {
    val sb = new StringBuffer
    for ((first: Int, subMap: Map[Int, V]) <- _map) {
      for ((second: Int, value: V) <- subMap) {
        sb.append("(%d, %d) => %s ".format(first, second, value.toString))
      }
    }
    sb.toString
  }

  def size: Int = _map.foldLeft(0)(_ + _._2.size)

  def keys: Iterable[Int] = _keys
}

class DensePairMap[V](val dim1: Int, val dim2: Int, val default: V)
                     (implicit manifest: Manifest[V]) extends PairMap[V] {
  def this(dim: Int, default: V)(implicit manifest: Manifest[V]) = this (dim, dim, default) (manifest)

  var _array: Array[Array[V]] = new Array(dim1)

  def keys = 0 until math.max(dim1, dim2)

  def size = dim1 * dim2

  def clear = _array = new Array(dim1)

  def get(i: Int, j: Int) = if (_array(i) != null && _array(i)(j) != null) Some(_array(i)(j)) else None

  def put(i: Int, j: Int, v: V): V = {
    if (_array(i) == null) _array(i) = Array.fill(dim2)(default)
    _array(i)(j) = v
    v
  }
}

object PairMapTest {
  def main(args: Array[String]) = {
    TimeUtil.init
    val maxNumbers = 10000
    val sparse: Boolean = (args.length == 1 && args(0).equalsIgnoreCase("sparse"))
    val pmap: PairMap[Double] = if (sparse) new SparsePairMap(false) else new DensePairMap(maxNumbers, maxNumbers, Double.NaN)
    for (i: Int <- 0 until maxNumbers)
      for (j: Int <- 0 until maxNumbers) {
        val value: Double = ((i + 1) * maxNumbers) + (j + 1)
        pmap.put(i, j, value)
      }
    TimeUtil.snapshot("Put")
    for (i: Int <- 0 until maxNumbers)
      for (j: Int <- 0 until maxNumbers) {
        val value: Double = ((i + 1) * maxNumbers) + (j + 1)
        assert(pmap.get(i, j).isDefined)
        assert(pmap.get(i, j).get == value, pmap.get(i, j) + " != " + value)
      }
    TimeUtil.snapshot("Got")
  }
}