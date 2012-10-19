package org.sameersingh.utils.timing

import java.util.Date
import collection.mutable.HashMap

class Snapshotter {
  var _initialTime: Long = 0
  var _snapshot: Long = 0

  def init = {
    _initialTime = new Date().getTime;
    _snapshot = new Date().getTime;
  }

  def snapshot(prefix: String): Unit = synchronized {
    val current = new Date().getTime
    println(prefix + ":: snapshot: " + format(current - _snapshot) + ", total: " + format(current - _initialTime))
    _snapshot = current
  }

  def format(milli: Long): String = {
    val seconds: Long = milli / 1000
    val minutes: Long = seconds / 60
    val hours: Long = minutes / 60
    new String("%d:%02d.%02d".format(hours, minutes % 60, seconds % 60))
  }

  def snapshot: Unit = snapshot("")

  def currentTime: Long = new Date().getTime - _initialTime

  def snapshotTime: Long = new Date().getTime - _snapshot

}

object TimeUtil extends Snapshotter {

  val _map: HashMap[AnyRef, Snapshotter] = new HashMap

  def apply[A <: AnyRef](ref: A): Snapshotter = _map.getOrElseUpdate(ref, {new Snapshotter})

  def main(argv: Array[String]): Unit = {

  }
}
