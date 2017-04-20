package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._

import com.github.agetakoyaki29.scala.mydouble.MyDouble


/**
 * 2 length Double Seq.
 */
abstract class Dim2(val x: Double, val y: Double) extends IndexedSeq[Double] with Dim {

  // ---- validation ----

  protected def isValid: Boolean = !isNaN && !isInfinite

  // ---- for IndexedSeq ----

  override final def foreach[U](f: Double => U): Unit = {f(x); f(y)}
  final def apply(idx: Int): Double = idx match { case 0 => x case 1 => y }
  final def length: Int = 2

}
