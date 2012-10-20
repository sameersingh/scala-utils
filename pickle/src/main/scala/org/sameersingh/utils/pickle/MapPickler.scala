package org.sameersingh.utils.pickle

import scala.collection.mutable.{HashMap,ArrayBuffer, Map}

/**
 * @author sameer
 * @date 10/19/12
 */

class PairPickler[Key, Val](implicit val keyPickler: Pickler[Key], val valPickler: Pickler[Val]) extends Pickler[(Key, Val)] {
  def saveToString(v: (Key, Val)) = {
    "%s:%s".format(keyPickler.saveToString(v._1), valPickler.saveToString(v._2))
  }

  def loadFromString(s: String) = {
    val elems = s.split(":")
    assert(elems.length == 2)
    (keyPickler.loadFromString(elems(0)), valPickler.loadFromString(elems(1)))
  }
}

class IterablePickler[K](implicit val elemPickler: Pickler[K]) extends Pickler[Iterable[K]] {
  def saveToString(v: Iterable[K]) = {
    v.map((k: K) => "%s".format(elemPickler.saveToString(k))).mkString(",")
  }

  def loadFromString(s: String) = {
    val counts: ArrayBuffer[K] = new ArrayBuffer
    for (split <- s.split(",")) {
      counts += elemPickler.loadFromString(split)
    }
    counts
  }
}

class ListPickler[K](implicit val elemPickler: Pickler[K]) extends Pickler[List[K]] {
  def saveToString(v: List[K]) = {
    v.map((k: K) => "%s".format(elemPickler.saveToString(k))).mkString(",")
  }

  def loadFromString(s: String) = {
    val counts: ArrayBuffer[K] = new ArrayBuffer
    for (split <- s.split(",")) {
      counts += elemPickler.loadFromString(split)
    }
    counts.toList
  }
}

/*class MapPickler[K,V](implicit val pairPickler: Pickler[(K,V)]) extends Pickler[Map[K,V]] {
  def saveToString(v: Map[K,V]) = {
    v.map((p: (K,V)) => "%s".format(pairPickler.saveToString(p))).mkString(",")
  }

  def loadFromString(s: String) = {
    val counts: HashMap[K,V] = new HashMap
    for (split <- s.split(",")) {
      val p = pairPickler.loadFromString(split)
      counts(p._1) = p._2
    }
    counts
  }
}*/
