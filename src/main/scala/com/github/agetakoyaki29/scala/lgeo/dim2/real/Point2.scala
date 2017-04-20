package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._

import scala.reflect.ClassTag


object Point2 extends Real2Factory[Point2] {
  def apply(x: Double, y: Double): Point2 = new Point2(x, y)

  val O: Point2 = Zero
}


final class Point2(_x: Double, _y: Double) extends Real2(_x, _y) with Vector2Like[Point2] with Trans2[Point2] {

  // ---- for CanFact ----

  val factory: Real2Factory[_ <: Point2] = Point2

  // ---- for Figure2 for pt ----

  /**
   * return distance between two point >= 0
   */
  def distance(op: Point2): Double = (this to op).norm
  def distanceSqr(op: Point2): Double = (this to op).normSqr

  def samePoint2(op: Point2) = this zip op forall tupled{_=~_}

  // ---- for Trans2 ----

  def from(op: Point2): Point2   = this.-(op)
  def unfrom(op: Point2): Point2 = this.+(op)

  // ---- use Trans2 ----

  // def to[A <: Trans2[A]](trans: A): A = trans from this
  def to[A : ClassTag](any: A): A = {
    case class TransA(val trans: Trans2[A])
    any match {
      case trans: Trans2[A] => trans from this
      case any => any
    }
  }

  // def unto[A <: Trans2[A]](trans: A): A = trans unfrom this
  def unto[A : ClassTag](any: A): A = {
    case class TransA(val trans: Trans2[A])
    any match {
      case trans: Trans2[A] => trans unfrom this
      case any => any
    }
  }

  // def conjugate[A, B](f: A => B): A => B = f match {
  //   case f3: Trans2[A] => Trans2[B] => f3 compose {unto(_: A)} andThen {to(_: B)}
  // }
  def conjugate[A : ClassTag, B : ClassTag](f: A => B): A => B = f compose {to(_: A)} andThen {unto(_: B)}

}
