package com.github.agetakoyaki29.scala.lgeo.dim2


trait Trans2[Repr] {
  def from(pt: Point2): Repr
  def unfrom(pt: Point2): Repr
}
