package org.sameersingh.utils.timing

import java.util.Date
import collection.mutable.HashMap

class Snapshotter {
  private var _initialTime: Long = 0
  private var _snapshot: Long = 0

  private var _accumTotal: Long = 0
  private var _paused: Boolean = false

  def init = {
    _initialTime = new Date().getTime
    _snapshot = new Date().getTime
    _accumTotal = 0
    _paused = false
  }

  def snapshot(prefix: String): Unit = synchronized {
    assert(!_paused)
    val snapshotTime = _takeSnapshot
    println(prefix + ":: snapshot: " + format(snapshotTime) + ", total: " + formatted)
  }

  private def _takeSnapshot: Long = {
    val current = new Date().getTime
    val r = current - _snapshot
    _snapshot = current
    r
  }

  private def format(milli: Long): String = {
    val seconds: Long = milli / 1000
    val minutes: Long = seconds / 60
    val hours: Long = minutes / 60
    new String("%d:%02d.%02d".format(hours, minutes % 60, seconds % 60))
  }

  def pause = {
    assert(!_paused)
    _accumTotal = currentTotal
    _paused = true
  }

  def resume = {
    assert(_paused)
    _initialTime = new Date().getTime
    _snapshot = _initialTime
    _paused = false
  }

  def snapshot: Unit = snapshot("")

  def currentTotal: Long = if(_paused) _accumTotal else (new Date().getTime - _initialTime) + _accumTotal

  def formatted: String = format(currentTotal)

  def snapshotTime: Long = new Date().getTime - _snapshot

}

object TimeUtil extends Snapshotter {

  val _map: HashMap[AnyRef, Snapshotter] = new HashMap

  def apply[A <: AnyRef](ref: A): Snapshotter = _map.getOrElseUpdate(ref, {new Snapshotter})

  def main(argv: Array[String]): Unit = {

  }
}
