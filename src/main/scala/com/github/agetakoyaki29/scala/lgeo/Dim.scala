package com.github.agetakoyaki29.scala.lgeo


/**
 * Double's Seq.
 */
trait Dim extends IndexedSeq[Double] {

  // val factory: DimFactory[_ <: Dim]

  final def isZero: Boolean = forall{_.isZero}
  final def isInfinite: Boolean = !isNaN && exists{_.isInfinite}
  final def isNaN: Boolean = exists{_.isNaN}

  final def norm: Double = normSqr.sqrt
  final def normSqr: Double = this map {d => d*d} sum  // this dot this

  // ---- std ----

  override def toString = this.getClass.getSimpleName + "(" + mkString(", ") + ")"  // for convenience

  override def equals(op: Any) = op match {
    case dim: Dim => (this.length == dim.length) && (this zip dim forall tupled{_==_})
    case _ => false
  }

  override def hashCode: Int = java.util.Arrays.hashCode(this.toArray)

}
