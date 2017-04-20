package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._


object Line2 {
  def apply(sp: Point2, dir: Dir2) = new Line2(sp, dir)

  def Connect(sp: Point2, ep: Point2) = new Line2(sp, Dir2(sp to ep))
}


final class Line2(val sp: Point2, val dir: Dir2) extends Trans2[Line2] with Figure2 {

  def updated(sp: Point2, dir: Dir2): Line2 = Line2(sp, dir)
  def updatedSP(sp: Point2): Line2 = updated(sp, dir)
  def updatedDir(dir: Dir2): Line2 = updated(sp, dir)
  def updatedEP(ep: Point2): Line2 = updatedDir(Dir2(sp to ep))

  // ---- copy from Dir2 (special) ----

  def reflect: Line2 = Line2(ep, dir.reflect)

  // ---- for Trans2 ----

  def from(op: Point2): Line2   = updatedSP(sp-op)
  def unfrom(op: Point2): Line2 = updatedSP(sp+op)

  // ---- for Figure2 ----

  def same(figure: Figure2): Boolean = figure match {
    case line: Line2 => this sameLine2 line
    case _ => false
  }

  def contain(op: Figure2): Boolean = op match {
    case line: Line2 => !(this isIntersect line) && (this containPoint2 line.sp)
    case circle: Circle2 => (this containPoint2 circle.center) && ((this distanceSqr circle.center) <~ circle.powerSqr)
    case aabb: AABB2 => aabb.points map {this containPoint2 _} forall identity
  }

  def isIntersect(op: Figure2): Boolean = op match {
    case line: Line2 => this isIntersectLine2 line
    case aabb: AABB2 => this isIntersectAABB2 aabb
    case _ => op isIntersect this
  }

  def intersect(op: Figure2): Seq[Point2] = op match {
    case line: Line2 => this intersectLine2 line
    case aabb: AABB2 => this intersectAABB2 aabb
    case _ => op intersect this
  }

  // ---- std ----

  override def toString: String = s"Line2($sp, $dir)"

  override def equals(op: Any) = op match {
    case line: Line2 => sp == line.sp && dir == line.dir
    case _ => false
  }

  override def hashCode: Int = 32*sp.## + dir.##

  // ---- copy from Dir2 ----

  def ep: Point2 = sp unto dir.ep
  def points: Seq[Point2] = dir.points map {_ unfrom sp}
  def aabb: AABB2 = sp unto dir.aabb

  def normal(op: Line2): Boolean = sp conjugate dir.normal apply op.dir
  def parallel(op: Line2): Boolean = sp conjugate dir.parallel apply op.dir
  def align(idx: Int): Boolean = sp conjugate dir.align apply idx
  //def angle: Double = sp unto dir.angle
  //def angleTo(op: Line2): Double = sp conjugate dir.angleTo apply op.dir
  //def cosTo(op: Line2): Double = sp conjugate dir.cosTo apply op.dir
  //def sinTo(op: Line2): Double = sp conjugate dir.sinTo apply op.dir

  def getThroughPt(t: Double): Point2 = sp unto dir.getThroughPt(t)
  def getParameter(pt: Point2): Double = sp conjugate dir.getParameter apply pt

  def inRegion1(pt: Point2): Boolean = sp conjugate dir.inRegion1 apply pt
  def inRegion2(pt: Point2): Boolean = sp conjugate dir.inRegion2 apply pt
  def through(pt: Point2): Boolean = sp conjugate dir.through apply pt
  def containPoint2(pt: Point2): Boolean = sp conjugate dir.containPoint2 apply pt
  def distance(pt: Point2): Double = sp conjugate dir.distance apply pt
  def distanceSqr(pt: Point2): Double = sp conjugate dir.distanceSqr apply pt
  def nearest(pt: Point2): Point2 = sp conjugate dir.nearest apply pt

  def sameLine2(line: Line2): Boolean = sp conjugate dir.sameLine2 apply line
  def isIntersectLine2(line: Line2): Boolean = sp conjugate dir.isIntersectLine2 apply line
  def isIntersectAABB2(aabb: AABB2): Boolean = sp conjugate dir.isIntersectAABB2 apply aabb
  def intersectLine2(line: Line2): Seq[Point2] = dir.intersectLine2(line from sp) map {_ unfrom sp}
  def intersectAABB2(aabb: AABB2): Seq[Point2] = dir.intersectAABB2(aabb from sp) map {_ unfrom sp}

}
