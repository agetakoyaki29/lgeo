package com.github.agetakoyaki29.scala.lgeo.dim2
import com.github.agetakoyaki29.scala.lgeo._

import scala.reflect.ClassTag


abstract class Real2Factory[T <: Real2 : ClassTag] extends Dim2Factory[T] {

  // ---- angle ----

  def AtAngle(angle: Double, power: Double = 1): T = {
    val (x, y) = (Math.cos(angle) * power, Math.sin(angle) * power)
    this.apply(x, y)
  }

}
