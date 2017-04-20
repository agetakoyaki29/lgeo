package com.github.agetakoyaki29.scala.lgeo


trait DimFactory[T <: Dim] {

  def Length: Int
  def SeqToDim(seq: Seq[Double]): T

  // ----

  final def AllOf(elem: Double): T = SeqToDim(Indices map {_ => elem})

  final val Indices: Range = 0 until Length
  //final def OtherIndices(idx: Int): IndexedSeq[Int] = Indices filter {_ != idx}

  final def Zero: T = AllOf(0d)
  final def Infinity: T = AllOf(Double.PositiveInfinity)
  final def NaN: T = AllOf(Double.NaN)

  final def E(idx: Int): T = SeqToDim(AllOf(0d) updated (idx, 1d))
  final def F(idx: Int): T = SeqToDim(AllOf(1d) updated (idx, 0d))

  // ---- for validation ----

  final def NotNaN:      T => T = dim => { require(! dim.isNaN,      "not NaN "      + dim.getClass.getSimpleName); dim }
  final def NotInfinite: T => T = dim => { require(! dim.isInfinite, "not Infinite " + dim.getClass.getSimpleName); dim }
  final def NotZero:     T => T = dim => { require(! dim.isZero,     "not Zero "     + dim.getClass.getSimpleName); dim }
  final def Identity:    T => T = identity

}
