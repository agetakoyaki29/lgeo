package com.github.agetakoyaki29.scala


package object lgeo {

  implicit def d2mrd(d: Double) = new com.github.agetakoyaki29.scala.mydouble.MyRichDouble(d)

  def tupled[T1, T2, R](func: Function2[T1, T2, R]) = Function.tupled(func) // equal (import Function.tupeld)

}
