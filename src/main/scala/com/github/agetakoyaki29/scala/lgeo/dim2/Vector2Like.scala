package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._

import com.github.agetakoyaki29.scala.mydouble.MyDouble


trait Vector2Like[Repr<:Dim2] extends Seq2Like[Repr] {

  // ---- basic operators ----

  final def unary_+(): Repr = factory(this)
  final def unary_-(): Repr = mapD2{-_}

  final def +(op: Dim2): Repr = zipmapD2(op) {_+_}

  final def -(op: Dim2): Repr = zipmapD2(op) {_-_}

  final def *(d: Double): Repr = this mapD2 {_*d}

  /**
   * @param d NotZero(zero / zero = NaN, any / zero = inf)
   */
  final def /(d: Double): Repr = {
    MyDouble.NotZero(d)
    this mapD2 {_/d}
  }

  // ----

  final def abs: Repr = mapD2{_.abs}

  final def minus: Repr = -this

  /**
   * return norm 1, same direction Dim2
   * isZero => throw IllegalStateException
   */
  final def normalized: Repr = {
    if(this.isZero) throw new IllegalStateException("Zero can't normalize")
    this / norm
  }

}
