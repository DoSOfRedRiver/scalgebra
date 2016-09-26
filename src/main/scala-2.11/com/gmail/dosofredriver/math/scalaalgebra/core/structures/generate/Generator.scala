package com.gmail.dosofredriver.math.scalaalgebra.core.structures.generate

import com.gmail.dosofredriver.math.scalaalgebra.core.structures.{ExtendedRing, Ring}

import scala.language.experimental.macros

/**
  * Created by DoSofRedRiver on 11.08.2016.
  */
object Generator {
  implicit def summonExtendedRing[T]: ExtendedRing[T] = macro StructuresGeneratorMacro.summonExtendedRing[T]
  implicit def summonRing[T]: Ring[T] = macro StructuresGeneratorMacro.summonRing[T]
}
