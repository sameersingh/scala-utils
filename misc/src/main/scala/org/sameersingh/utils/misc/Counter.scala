package org.sameersingh.utils.misc

/**
 * A simple counter class that can be disabled, whereby calls to update have to effect
 * @author sameer
 */
class Counter {

  private var _enabled: Boolean = false
  private var _total: Long = 0

  /** Try to increment the counter. If it is disabled, this call will have no effect
   * @param i Amount to increase the counter by
   */
  def +=(i:Int): Unit = if (enabled) _total += i

  /** Whether the counter is enabled or not */
  def enabled = _enabled

  /** Enable the counter */
  def enable = _enabled = true

  /** Disable the counter */
  def disable = _enabled = false

  /** Current running total */
  def total = _total
}
