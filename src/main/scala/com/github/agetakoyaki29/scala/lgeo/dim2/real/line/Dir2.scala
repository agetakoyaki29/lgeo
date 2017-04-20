package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._


object Dir2 extends Real2Factory[Dir2] {
  def apply(x: Double, y: Double): Dir2 = new Dir2(x, y)
}


final class Dir2(_x: Double, _y: Double) extends Real2(_x, _y) with Vector2Like[Dir2] {

  // ---- for CanFact ----

  val factory: Real2Factory[_ <: Dir2] = Dir2

  // ---- validation ----

  override protected def isValid: Boolean = super.isValid && !isZero

  // ----

  def toPoint2: Point2 = Point2(this)

  def toLine2: Line2 = Line2(Point2.O, this)

  // ---- points, aabb ----

  def sp: Point2 = Point2.O
  def ep: Point2 = this.toPoint2

  def points: Seq[Point2] = Seq(sp, ep)

  def aabb: AABB2 = AABB2.Whole

  def reflect: Dir2 = minus

  // ---- normal parallel ----

  def normal(op: Dir2): Boolean = this dotEq0 op
  def parallel(op: Dir2): Boolean = this crossEq0 op

  def align(idx: Int): Boolean = this parallel Dir2.E(idx)

  def normalDir2: Dir2 = factory(-y, x)

  // ---- parameter ----

  def getThroughPt(t: Double): Point2 = this.toPoint2 * t

  def getParameter(pt: Point2): Double = if(!(this through pt)) Double.NaN else pt.norm / this.norm

  // ---- for pt ----

  def inRegion1(pt: Point2): Boolean = this dotGt0 pt
  def inRegion2(pt: Point2): Boolean = (this reflect) inRegion1 (pt - this)

  def through(pt: Point2): Boolean = this crossEq0 pt

  /**
   * 0 <= this angle pt <= pi
   * (this sinTo pt) < 0
   */
  def containPoint2(pt: Point2): Boolean = this crossLt0 pt

  /**
   * this sinTo pt * pt.norm
   */
  def distance(pt: Point2): Double = {
    if(this through pt) 0
    else (this cross pt / this.norm).abs
  }
  def distanceSqr(pt: Point2): Double = {
    if(this through pt) 0
    else (this cross pt).sqr / this.normSqr
  }

  /**
   * this.normalized * this cosTo pt * pt.norm
   * pt + this.normal.normalized * -distance
   * this * (this dot pt) / (this dot this)
   */
  def nearest(pt: Point2): Point2 = {
    if(this through pt) pt
    else this.toPoint2 * (this dot pt / this.normSqr)
  }

  // ---- for figure ----

  def sameDir2(op: Dir2): Boolean = this parallel op
  def sameLine2(line: Line2): Boolean = (this through line.sp) && (this sameDir2 line.dir)

  def isIntersectLine2(line: Line2): Boolean = ! (this parallel line.dir)
  def intersectLine2(line: Line2): Seq[Point2] = intersectTimeLine2(line) map {this.toPoint2 * _}
  def intersectTimeLine2(line: Line2): Seq[Double] = {
    if(!(this isIntersectLine2 line)) Seq.empty[Double]
    else Seq( (line.sp cross line.dir) / (this cross line.dir) )
  }

  def isIntersectAABB2(aabb: AABB2): Boolean = false  // TODO
  def intersectAABB2(aabb: AABB2): Seq[Point2] = intersectTimeAABB2(aabb) map {this.toPoint2 * _}
  def intersectTimeAABB2(aabb: AABB2): Seq[Double] = Seq.empty[Double] // TODO

}
