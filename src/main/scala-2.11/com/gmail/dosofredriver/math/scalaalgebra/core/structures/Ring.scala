package com.gmail.dosofredriver.math.scalaalgebra.core.structures

import scala.annotation.implicitNotFound
import scala.language.experimental.macros

/**
 * Created by Александр on 01.07.2015.
 */

@implicitNotFound("The implicit 'Ring' operations implementation for following class not found!")
trait Ring [T] {
  def plus(value: T, arg: T): T
  def minus(value: T, arg: T): T
  def multiply(value: T, arg: T): T
}

object Ring {
  implicit class RingOps[T : Ring](origin: T) {

    def |+| (arg: T) = implicitly[Ring[T]].plus(origin, arg)

    def |-| (arg: T) = implicitly[Ring[T]].minus(origin, arg)

    def |*| (arg: T) = implicitly[Ring[T]].multiply(origin, arg)
  }
}