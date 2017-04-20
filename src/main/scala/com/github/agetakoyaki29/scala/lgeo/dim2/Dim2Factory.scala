package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._

import scala.reflect.ClassTag


abstract class Dim2Factory[T <: Dim2 : ClassTag] extends DimFactory[T] {

  def apply(x: Double, y: Double): T

  // ----

  final def apply(op: Dim2): T = Clone(op)

  final def Clone(op: Dim2): T = this.apply(op.x, op.y)

  // ---- for DimFactory ----

  final def Length: Int = 2

  final def SeqToDim(seq: Seq[Double]): T = {
    require(seq.length == Length, s"wrong length seq; found: ${seq.length}, required: ${Length}")
    this(seq(0), seq(1))
  }

}


trait CanFact2[Repr<:Dim2] extends Dim2 {
  def factory: Dim2Factory[_<:Repr]
}
