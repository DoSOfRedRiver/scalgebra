package com.gmail.dosofredriver.math.scalaalgebra.core.structures

import scala.language.experimental.macros

/**
 * Created by Александр on 22.09.2015.
 */
trait ExtendedRing [T] extends Ring[T] {
  def compare(value: T, arg: T) : Boolean = value.equals(arg)
  def mod(value: T, arg: T) : T
  def div(value: T, arg: T) : T
  def zeroElem: T
  def neutralElem: T
}

object ExtendedRing {
  implicit class ExtendedRingOps[T : ExtendedRing](origin: T) {

    def |%| (arg: T) = implicitly[ExtendedRing[T]].mod(origin, arg)

    def |/| (arg: T) = implicitly[ExtendedRing[T]].div(origin, arg)

    def |==| (arg: T) = implicitly[ExtendedRing[T]].compare(origin, arg)
  }

  def _zero_ [T : ExtendedRing] = implicitly[ExtendedRing[T]].zeroElem
  def _neutral_ [T : ExtendedRing] = implicitly[ExtendedRing[T]].neutralElem
}
