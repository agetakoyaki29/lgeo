package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._


object Circle2 {
  def apply(center: Point2, range: Range2) = new Circle2(center, range)

  def Connect(center: Point2, ep: Point2) = new Circle2(center, Range2(center to ep))
}


final class Circle2(val center: Point2, val range: Range2) extends Trans2[Circle2] with Figure2 {

  def updated(center: Point2, range: Range2): Circle2 = Circle2(center, range)
  def updatedCenter(center: Point2): Circle2 = updated(center, range)
  def updatedRange(range: Range2): Circle2 = updated(center, range)
  def updatedEP(ep: Point2): Circle2 = updatedRange(Range2(center to ep))

  // ---- for Trans2 ----

  def from(op: Point2): Circle2   = updatedCenter(center-op)
  def unfrom(op: Point2): Circle2 = updatedCenter(center+op)

  // ---- for Figure2 ----

  def same(figure: Figure2): Boolean = figure match {
    case circle: Circle2 => this sameCircle2 circle
    case _ => false
  }

  def contain(op: Figure2): Boolean = op match {
    case line: Line2 => false
    case circle: Circle2 => this containCircle2 circle
    case aabb: AABB2 => aabb.points map {this containPoint2 _} forall identity
  }

  def isIntersect(op: Figure2): Boolean = op match {
    case line: Line2 => this isIntersectLine2 line
    case circle: Circle2 => this isIntersectCircle2 circle
    case _ => op isIntersect this
  }

  def intersect(op: Figure2): Seq[Point2] = op match {
    case line: Line2 => this intersectLine2 line
    case circle: Circle2 => this intersectCircle2 circle
    case _ => op intersect this
  }

  // ---- std ----

  override def toString: String = s"Circle2($center, $range)"

  override def equals(op: Any) = op match {
    case circle: Circle2 => center == circle.center && range == circle.range
    case _ => false
  }

  override def hashCode: Int = 32*center.## + range.##

  // ---- copy from Range2 ----

  def ep = center unto range.ep
  def points: Seq[Point2] = range.points map {_ unfrom center}
  def aabb: AABB2 = center unto range.aabb

  def power: Double = center unto range.power
  def powerSqr: Double = center unto range.powerSqr
  def isConcentric(circle: Circle2): Boolean = center conjugate range.isConcentric apply circle
  def radicalLine(circle: Circle2): Line2 = center conjugate range.radicalLine apply circle

  def getThroughPt(t: Double): Point2 = center unto range.getThroughPt(t)
  def getParameter(pt: Point2): Double = center conjugate range.getParameter apply pt
  def around(size: Int): Seq[Point2] = range.around(size) map {_ unfrom center}

  def through(pt: Point2): Boolean = center conjugate range.through apply pt
  def containPoint2(pt: Point2): Boolean = center conjugate range.containPoint2 apply pt
  def containCircle2(circle: Circle2): Boolean = center conjugate range.containCircle2 apply circle
  def distance(pt: Point2): Double = center conjugate range.distance apply pt
  def distanceSqr(pt: Point2): Double = center conjugate range.distanceSqr apply pt
  def nearest(pt: Point2): Point2 = center conjugate range.nearest apply pt

  def sameCircle2(circle: Circle2): Boolean = center conjugate range.sameCircle2 apply circle
  def isIntersectLine2(line: Line2): Boolean = center conjugate range.isIntersectLine2 apply line
  def isIntersectCircle2(circle: Circle2): Boolean = center conjugate range.isIntersectCircle2 apply circle
  def intersectLine2(line: Line2): Seq[Point2] = range.intersectLine2(line from center) map {_ unfrom center}
  def intersectCircle2(circle: Circle2): Seq[Point2] = range.intersectCircle2(circle from center) map {_ unfrom center}
}
