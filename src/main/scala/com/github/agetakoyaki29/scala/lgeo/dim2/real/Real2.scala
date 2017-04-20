package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._


/**
 * real 2-space
 */
abstract class Real2(_x: Double, _y: Double) extends Dim2(_x, _y) {

  // ---- dot, cross ----

  /**
   * return dot
   */
  final def dot(op: Real2): Double = this zip op map tupled{_*_} sum

  final def dotEq0(op: Real2): Boolean = x*op.x =~ - y*op.y
  final def dotGt0(op: Real2): Boolean = x*op.x >~ - y*op.y
  final def dotLt0(op: Real2): Boolean = x*op.x <~ - y*op.y

  /**
   * return No 3 elem of cross
   * Vector3(this) cross Vector3(op) apply 2
   */
  final def cross(op: Real2): Double = x*op.y - y*op.x

  final def crossEq0(op: Real2): Boolean = x*op.y =~ y*op.x
  final def crossGt0(op: Real2): Boolean = x*op.y >~ y*op.x
  final def crossLt0(op: Real2): Boolean = x*op.y <~ y*op.x

  // ---- angle ----

  /**
   * -pi ~ pi
   */
  def angle: Double = Math.atan2(y, x)

  def angleTo(op: Real2): Double = op.angle - this.angle

  def cosTo(op: Real2): Double = (this dot op) / this.norm / op.norm

  def sinTo(op: Real2): Double = (this cross op) / this.norm / op.norm

}
