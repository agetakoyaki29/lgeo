package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._


object Corner2 extends Real2Factory[Corner2] {
  def apply(x: Double, y: Double): Corner2 = new Corner2(x, y)

  def Whole: Corner2 = new Corner2(Double.MaxValue, Double.MaxValue) {
    override def containPoint2(pt: Point2): Boolean = true
    override def toString: String = "Whole"
  }
}


class Corner2(_x: Double, _y: Double) extends Real2(_x.abs, _y.abs) with Seq2Like[Corner2] {

  // ---- for CanFact ----

  override val factory: Real2Factory[_ <: Corner2] = Corner2

  // ----

  def toAABB2: AABB2 = AABB2(Point2.O, this)

  // ---- points, aabb ----

  def center: Point2 = Point2.O
  def ep: Point2 = Point2(this)
  def minmin: Point2 = Point2(-x, -y)
  def minmax: Point2 = Point2(-x,  y)
  def maxmin: Point2 = Point2( x, -y)
  def maxmax: Point2 = Point2( x,  y)

  def points: Seq[Point2] = Seq(center, minmin, minmax, maxmin, maxmax)

  def aabb: AABB2 = toAABB2

  // ---- border ----

  // def border(idx: Int): Border = ???
  // def borders: Seq[Border] = indices map {border _}
  // def bordersOther(border: Border): Seq[Border] = bordersOther(border.idx)
  // def bordersOther(idx: Int): Seq[Slab] = indicesOther(idx) map {border _}
  // def slab(idx: Int): Slab = ???
  // def slabsOther(slab: Slab): Seq[Slab] = slabsOther(slab.idx)
  // def slabsOther(idx: Int): Seq[Slab] = indicesOther(idx) map {slab _}
  // def slabs: Seq[Slab] = indices map {slab _}

  // ----

  def isConcentric(aabb: AABB2): Boolean = center samePoint2 aabb.center

  // ---- parameter ----

  def getThroughPt(t: Double): Point2 = ???      // TODO

  def getParameter(pt: Point2): Double = ???     // TODO

  // ---- for pt ----

  def through(pt: Point2): Boolean = {
    if(pt.x.abs =~ x) pt.y.abs <~ y else
    if(pt.y.abs =~ y) pt.x.abs <~ x else
    false
  }

  def containPoint2(pt: Point2): Boolean = this zip pt.abs forall tupled{_>~_}

  def distance(pt: Point2): Double = distanceSqr(pt).sqrt
  def distanceSqr(pt: Point2): Double = {
    val distance = -pt.abs + this
    if(distance forall {_ >= 0}) distance.min.sqr
    else distance filterNot {_ >= 0} map {_.sqr} sum
  }

  def nearest(pt: Point2): Point2 = {
    def toIdxMap(seq: Seq[Double]): Map[Int, Double] = (seq.indices zip seq).toMap
    val distance = -pt.abs + this
    val ideaMap = toIdxMap(this zip pt map tupled{_ copySign _})
    if(distance forall {_ >= 0}) {
      val minElem = toIdxMap(distance) minBy {_._2}
      val update = ideaMap -- (ideaMap.keySet -  minElem._1)
      (pt /: update) {(p, t) => (p.updatedD2 _).tupled(t)}  // (pt /: update) {_ updated _}
    } else {
      val outMap = toIdxMap(distance) filterNot {_._2 >= 0}
      val update = ideaMap -- (ideaMap.keySet &~ outMap.keySet)
      (pt /: update) {(p, t) => (p.updatedD2 _).tupled(t)}  // (pt /: update) {_ updated _}
    }
  }

  // ---- for figure ----

  def sameCorner2(op: Corner2): Boolean = this zip op forall tupled{_=~_}

  def sameAABB2(aabb: AABB2): Boolean = (this isConcentric aabb) && (this sameCorner2 aabb.corner)

  def isIntersectCircle2(circle: Circle2): Boolean = false  // TODO
  def intersectCircle2(circle: Circle2): Seq[Point2] = Seq.empty[Point2]  // TODO

  def isIntersectAABB2(aabb: AABB2): Boolean = false  // TODO
  def intersectAABB2(aabb: AABB2): Seq[Point2] = Seq.empty[Point2]  // TODO

}
