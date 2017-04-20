package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._


object AABB2 {
  def apply(center: Point2, corner: Corner2) = new AABB2(center, corner)

  def Connect(center: Point2, ep: Point2) = new AABB2(center, Corner2(center to ep))

  def Whole: AABB2 = new AABB2(Point2.O, Corner2.Whole) {
    override def updatedCenter(center: Point2): AABB2 = this
    override def contain(figure: Figure2): Boolean = true
    override def toString: String = "Whole"
  }
}


class AABB2(val center: Point2, val corner: Corner2) extends Trans2[AABB2] with Figure2 {

  def updated(center: Point2, corner: Corner2): AABB2 = AABB2(center, corner)
  def updatedCenter(center: Point2): AABB2 = updated(center, corner)
  def updatedCorner(corner: Corner2): AABB2 = updated(center, corner)
  def updatedEP(ep: Point2): AABB2 = updatedCorner(Corner2(center to ep abs))

  // ---- for Trans2 ----

  def from(op: Point2): AABB2   = updatedCenter(center-op)
  def unfrom(op: Point2): AABB2 = updatedCenter(center+op)

  // ---- for Figure2 ----

  def same(figure: Figure2): Boolean = figure match {
    case aabb: AABB2 => this sameAABB2 aabb
    case _ => false
  }

  def contain(op: Figure2): Boolean = op match {
    case line: Line2 => false
    case circle: Circle2 => (this containPoint2 circle.center) && ((this distanceSqr circle.center) <~ circle.powerSqr)
    case aabb: AABB2 => aabb.points map {this containPoint2 _} forall identity
  }

  def isIntersect(op: Figure2): Boolean = op match {
    case circle: Circle2 => this isIntersectCircle2 circle
    case aabb: AABB2     => this isIntersectAABB2 aabb
    case _ => op isIntersect this
  }

  def intersect(op: Figure2): Seq[Point2] = op match {
    case circle: Circle2 => this intersectCircle2 circle
    case aabb: AABB2     => this intersectAABB2 aabb
    case _ => op intersect this
  }

  // ---- std ----

  override def toString: String = s"AABB2($center, $corner)"

  override def equals(op: Any) = op match {
    case aabb: AABB2 => center == aabb.center && corner == aabb.corner
    case _ => false
  }

  override def hashCode: Int = 32*center.## + corner.##

  // ---- copy from Corner2 ----

  def ep: Point2 = center unto corner.ep
  def minmin: Point2 = center unto corner.minmin
  def minmax: Point2 = center unto corner.minmax
  def maxmin: Point2 = center unto corner.maxmin
  def maxmax: Point2 = center unto corner.maxmax
  def points: Seq[Point2] = corner.points map {_ unfrom center}
  def aabb: AABB2 = center unto corner.aabb

  def isConcentric(aabb: AABB2): Boolean = center conjugate corner.isConcentric apply aabb

  def getThroughPt(t: Double): Point2 = center unto corner.getThroughPt(t)
  def getParameter(pt: Point2): Double = center conjugate corner.getParameter apply pt

  def through(pt: Point2): Boolean = center conjugate corner.through apply pt
  def containPoint2(pt: Point2): Boolean = center conjugate corner.containPoint2 apply pt
  def distance(pt: Point2): Double = center conjugate corner.distance apply pt
  def distanceSqr(pt: Point2): Double = center conjugate corner.distanceSqr apply pt
  def nearest(pt: Point2): Point2 = center conjugate corner.nearest apply pt

  def sameAABB2(aabb: AABB2): Boolean = center conjugate corner.sameAABB2 apply aabb
  def isIntersectCircle2(circle: Circle2): Boolean = center conjugate corner.isIntersectCircle2 apply circle
  def isIntersectAABB2(aabb: AABB2): Boolean = center conjugate corner.isIntersectAABB2 apply aabb
  def intersectCircle2(circle: Circle2): Seq[Point2] = corner.intersectCircle2(circle from center) map {_ unfrom center}
  def intersectAABB2(aabb: AABB2): Seq[Point2] = corner.intersectAABB2(aabb from center) map {_ unfrom center}

}
