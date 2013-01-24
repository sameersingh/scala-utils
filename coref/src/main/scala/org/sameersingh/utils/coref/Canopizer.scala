package org.sameersingh.utils.coref

import collection.mutable.{ArrayBuffer, HashSet}

/**
 * @author sameer
 */
abstract class Canopizer[R <: MentionRecord] {
  def canopies(r: R): scala.collection.Set[String]

  def canopies(rs: Iterable[R]): scala.collection.Set[String] = {
    val result = new HashSet[String]
    rs.foreach(result ++= canopies(_))
    result
  }
}

class DefaultCanopizer[R <: MentionRecord] extends Canopizer[R] {
  def canopies(r: R) = r.defaultCanopies
}

class Canopizers[R <: MentionRecord](canopizers: Canopizer[R]*) extends Canopizer[R] {
  private val _canopizers = new ArrayBuffer[Canopizer[R]]
  _canopizers ++= canopizers

  def canopies(r: R) = {
    val result = new HashSet[String]
    canopizers.foreach(result ++= _.canopies(r))
    result
  }
}