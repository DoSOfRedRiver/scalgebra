package com.gmail.dosofredriver.math.scalaalgebra.core.impl

import com.gmail.dosofredriver.math.scalaalgebra.core.algo.RingAlgo
import com.gmail.dosofredriver.math.scalaalgebra.core.structures.ExtendedRing
import com.gmail.dosofredriver.math.scalaalgebra.core.structures.generate.Generator

/**
 * Created by Александр on 23.07.2015.
 */
class IntegerModulo (var value: Int, var radix: Int = 32) {
  value = if (value == radix) radix else (value + radix) % radix

  def + (arg: IntegerModulo) : IntegerModulo = {
    new IntegerModulo((value + arg.value) % radix, radix)
  }

  def - (arg: IntegerModulo) : IntegerModulo = {
    new IntegerModulo((value - arg.value + radix) % radix, radix)
  }

  def * (arg: IntegerModulo) : IntegerModulo = {
    new IntegerModulo(value * arg.value % radix, radix)
  }

  def % (arg: IntegerModulo) : IntegerModulo = {
    new IntegerModulo(value % arg.value, radix)
  }

  def / (arg: IntegerModulo) : IntegerModulo = {
    new IntegerModulo((value - (value % arg.value)) / arg.value, radix)
  }

  def == (arg: IntegerModulo) : Boolean = {
    value == arg.value
  }

  def inverse = {
    //TODO: remove this declaration
    implicit val a = IntegerModulo.IntegerModuloExtRing

    val res = RingAlgo.gcdex(this, getModulo)

    if (res._1.value == 1)
      new IntegerModulo(res._2.value)
    else
      null
  }

  def normalize: IntegerModulo = {
    new IntegerModulo(value % radix)
  }

  private def getModulo : IntegerModulo = {
    new IntegerModulo(radix)
  }

  override def toString() : String = {
    value.toString + "::" + radix
  }
}

object IntegerModulo {
  def apply(arg: Int) : IntegerModulo = {
    new IntegerModulo(arg)
  }

  def getType(radix: Int) = {
    new IntegerModulo(_: Int, radix)
  }


  implicit object IntegerModuloExtRing extends ExtendedRing[IntegerModulo] {
    override def mod(value: IntegerModulo, arg: IntegerModulo): IntegerModulo = value % arg

    override def div(value: IntegerModulo, arg: IntegerModulo): IntegerModulo = value / arg

    override def zeroElem: IntegerModulo = new IntegerModulo(0)

    override def neutralElem: IntegerModulo = new IntegerModulo(1)

    override def plus(value: IntegerModulo, arg: IntegerModulo): IntegerModulo = value + arg

    override def multiply(value: IntegerModulo, arg: IntegerModulo): IntegerModulo = value * arg

    override def minus(value: IntegerModulo, arg: IntegerModulo): IntegerModulo = value - arg
  }


  implicit def intMod2Int(arg: IntegerModulo) = {
    arg.value
  }

  implicit def int2IntMod(arg: Int) = {
    new IntegerModulo(arg)
  }
}