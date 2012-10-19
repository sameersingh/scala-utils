package org.sameersingh.utils.misc

import collection.mutable.{Map, HashMap}

/**
 * Map from string names to long ids
 * @author sameer
 * @date Nov 13, 2010
 */

class Alphabet {
  val indices_ : Map[String, Long] = new HashMap
  val names_ : Map[Long, String] = new HashMap

  var frozen_ : Boolean = false

  def apply(name:String): Long = index(name)

  def index(name: String): Long = {
    indices_.get(name) match {
      case Some(v) => v
      case None => {
        if (!frozen_) {
          val d = indices_.size;
          indices_(name) = d;
          names_(d) = name
          d
        } else -1
      }
    }
  }

  def name(id: Long): String = names_.getOrElse(id, "")

  def freeze: Unit = {
    frozen_ = true
  }

  def clear = {
    frozen_ = false
    indices_.clear
    names_.clear
  }

}
