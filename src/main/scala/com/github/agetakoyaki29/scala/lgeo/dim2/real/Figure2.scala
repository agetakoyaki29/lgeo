package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._


trait Figure2 {

  // ---- points, aabb ----

  def points: Seq[Point2]
  // def aabb: AABB2

  // ---- parameter ----

  def getThroughPt(t: Double): Point2
  def getParameter(pt: Point2): Double

  // ---- for pt ----

  def distance(pt: Point2): Double
  def distanceSqr(pt: Point2): Double
  def nearest(pt: Point2): Point2
  def through(pt: Point2): Boolean
  def containPoint2(pt: Point2): Boolean

  // ---- for figure ----

  def same(op: Figure2): Boolean
  def contain(op: Figure2): Boolean
  def isIntersect(op: Figure2): Boolean
  def intersect(op: Figure2): Seq[Point2]

}
