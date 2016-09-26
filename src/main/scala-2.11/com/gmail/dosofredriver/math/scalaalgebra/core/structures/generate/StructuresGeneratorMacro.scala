package com.gmail.dosofredriver.math.scalaalgebra.core.structures.generate

import com.gmail.dosofredriver.math.scalaalgebra.core.structures.{ExtendedRing, Ring}

import scala.reflect.macros.blackbox.Context


/**
 * Created by Александр on 22.09.2015.
 */
object StructuresGeneratorMacro {
  def summonRing[T: ctx.WeakTypeTag](ctx: Context): ctx.Expr[Ring[T]] = {
    import ctx.universe._

    val T = weakTypeOf[T]

    ctx.Expr[Ring[T]](q"""
      new Ring[$T] {
        override def plus(value: $T, arg: $T) : $T = value + arg
        override def minus(value: $T, arg: $T) : $T = value - arg
        override def multiply(value: $T, arg: $T) : $T = value * arg
      }
    """)
  }

  def summonExtendedRing[T: ctx.WeakTypeTag](ctx: Context): ctx.Expr[ExtendedRing[T]] = {
    import ctx.universe._

    println("No implicit implementation found. Trying to generify..")

    val T = weakTypeOf[T]

    ctx.Expr[ExtendedRing[T]](q"""
      new ExtendedRing[$T] {
        override def plus(value: $T, arg: $T) : $T = value + arg
        override def minus(value: $T, arg: $T) : $T = value - arg
        override def multiply(value: $T, arg: $T) : $T = value * arg
        override def mod(value: $T, arg: $T): $T = value % arg
        override def div(value: $T, arg: $T): $T = (value - (value % arg)) / arg
        override def compare(value: $T, arg: $T) : Boolean = value.equals(arg)
        override def zeroElem = new $T(0)
        override def neutralElem = new $T(1)
      }
    """)
  }
}
