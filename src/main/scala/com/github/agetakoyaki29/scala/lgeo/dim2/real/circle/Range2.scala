package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._


object Range2 extends Real2Factory[Range2] {
  def apply(x: Double, y: Double): Range2 = new Range2(x, y)
}


final class Range2(_x: Double, _y: Double) extends Real2(_x, _y) with Vector2Like[Range2] {

  // ---- for CanFact ----

  val factory: Real2Factory[_ <: Range2] = Range2

  // ---- validation ----

  override protected def isValid: Boolean = super.isValid && !isZero

  // ----

  def toCircle2: Circle2 = Circle2(Point2.O, this)

  // ---- points, aabb ----

  def center: Point2 = Point2.O
  def ep: Point2 = Point2(this)

  def points: Seq[Point2] = Seq(center, ep)

  def aabb: AABB2 = AABB2(Point2.O, Corner2(norm, norm))

  // ----

  def sa: Double = angle

  def power: Double = this.norm
  def powerSqr: Double = this.normSqr

  def isConcentric(circle: Circle2): Boolean = center samePoint2 circle.center

  def radicalLine(circle: Circle2): Line2 = {
    require(!(this isConcentric circle), "not concentric Circle2")
    val opcenter = circle.center
    val radicalPoint = opcenter * ( ((this.powerSqr-circle.powerSqr)/opcenter.normSqr + 1) / 2 )
    Line2(radicalPoint, Dir2(opcenter).normalDir2)
  }

  // ---- parameter ----

  def getThroughPt(t: Double): Point2 = Point2.AtAngle(this.angle + t) * power

  def getParameter(pt: Point2): Double = if(!(this through pt)) Double.NaN else this angleTo pt

  def around(size: Int): Seq[Point2] =
    if(size <= 0) Seq.empty[Point2]
    else Seq.tabulate(size) {_.toDouble}
      .map {_ * 2 * Math.PI / size}
      .map {getThroughPt _}

  // ---- for pt ----

  def through(pt: Point2): Boolean = this.normSqr =~ pt.normSqr

  def containPoint2(pt: Point2): Boolean = this.normSqr >~ pt.normSqr

  def distance(pt: Point2): Double = {
    if(this through pt) 0
    else (pt.norm - this.norm).abs
  }
  def distanceSqr(pt: Point2): Double = {
    if(this through pt) 0
    else distance(pt).sqr
  }

  def nearest(pt: Point2): Point2 = {
    if(this through pt) pt
    // require(!(this.center samePoint2 pt), "not center Point2")
    else if(this.center samePoint2 pt) Point2(this)   // TODO
    else pt * (this.norm / pt.norm)
  }

  // ---- for figure ----

  def sameRange2(op: Range2): Boolean = this.powerSqr =~ op.powerSqr
  def sameCircle2(circle: Circle2): Boolean = (this isConcentric circle) && (this sameRange2 circle.range)

  def containCircle2(circle: Circle2): Boolean    = this.power >~ circle.center.norm + circle.power
  def notContainCircle2(circle: Circle2): Boolean = this.power <~ circle.center.norm - circle.power

  def isIntersectLine2(line: Line2): Boolean = this.powerSqr >~ (line distanceSqr Point2.O)
  def intersectLine2(line: Line2): Seq[Point2] = {
    val nearest = line nearest Point2.O
    if(!(this containPoint2 nearest)) Seq.empty[Point2]
    else if(this through nearest) Seq(nearest)
    else {
      val sine = (this.normSqr - nearest.normSqr).sqrt
      val diff = line.dir.normalized * sine
      Seq(nearest+diff, nearest-diff)
    }
  }

  def isIntersectCircle2(circle: Circle2): Boolean =
    !(this isConcentric circle) &&
    (this.power - circle.center.norm).abs <~ circle.power // circle contain (circle nearest O)
  def intersectCircle2(circle: Circle2): Seq[Point2] = {
    if(this isConcentric circle) Seq.empty[Point2]
    else this intersectLine2 (this radicalLine circle)
  }

}
