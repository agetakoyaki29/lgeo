package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._


trait Seq2Like[Repr<:Dim2] extends CanFact2[Repr] {

  final def reverseD2: Repr = factory.SeqToDim(super.reverse)

  final def updatedD2(idx: Int, elem: Double): Repr = factory.SeqToDim(super.updated(idx, elem))

  final def mapD2(f: Double => Double): Repr = factory.SeqToDim(super.map(f))

  final def zipmapD2(op: Dim2)(f: (Double, Double) => Double): Repr = factory.SeqToDim(this zip op map f.tupled)

}
