package org.sameersingh.utils.pickle

import java.awt.RenderingHints.Key

/**
 * @author sameer
 * @date 10/19/12
 */
object Implicits {
  implicit val intPickler = IntPickler
  implicit val stringPickler = StringPickler
  implicit val doublePickler = DoublePickler

  implicit def pairPickler[K, V](implicit keyPickler: Pickler[K], valuePickler: Pickler[V]): Pickler[(K, V)] = new PairPickler[K, V]

  implicit def iterablePickler[K](implicit elemPickler: Pickler[K]): Pickler[Iterable[K]] = new IterablePickler[K]

  implicit def listPickler[K](implicit elemPickler: Pickler[K]): Pickler[List[K]] = new ListPickler[K]
  //implicit def mapPickler[K, V](implicit pairPickler: Pickler[(K, V)]) = new MapPickler[K, V]

  def pickle(v: Int)= intPickler.saveToString(v)
  def pickle(v: Double) = doublePickler.saveToString(v)
  def pickle(v: String) = stringPickler.saveToString(v)
  def pickle[K,V](p: (K,V))(implicit pickler: Pickler[(K,V)]) = pickler.saveToString(p)
  def pickle[K](v: Iterable[K])(implicit pickler: Pickler[Iterable[K]]) = pickler.saveToString(v)

  def unpickle(str: String, v: Int)= intPickler.loadFromString(str)
  def unpickle(str: String, v: Double) = doublePickler.loadFromString(str)
  def unpickle(str: String, v: String) = stringPickler.loadFromString(str)
  def unpickle[K,V](str: String, p: (K,V))(implicit pickler: Pickler[(K,V)]) = pickler.loadFromString(str)
  def unpickle[K](str: String, v: Iterable[K])(implicit pickler: Pickler[Iterable[K]]) = pickler.loadFromString(str)
  def unpickle[V](str: String)(implicit pickler: Pickler[V]) = pickler.loadFromString(str)
}
