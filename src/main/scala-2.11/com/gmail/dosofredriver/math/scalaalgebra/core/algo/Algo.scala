package com.gmail.dosofredriver.math.scalaalgebra.core.algo

import com.gmail.dosofredriver.math.scalaalgebra.core.impl.IntegerModulo
import com.gmail.dosofredriver.math.scalaalgebra.core.structures.{ExtendedRing, Ring}

import scala.annotation.{implicitNotFound, tailrec}

/**
 * Created by Александр on 09.08.2015.
 */
@implicitNotFound("The implicit implementations of required algorithms not found!")
trait Algo {
  def gcd[T : ExtendedRing](arg1: T, arg2: T) : T
  def gcdex[T : ExtendedRing](arg1: T, arg2: T) : Tuple3[T, T, T]
}

object RingAlgo {
    /*@tailrec def gcd(arg1: IntegerModulo, arg2: IntegerModulo) : IntegerModulo = {
      if (arg2 == 0) arg1 else gcd(arg2, arg1%arg2)
    }*/

    /*def gcdex(arg1: IntegerModulo, arg2: IntegerModulo) : Tuple3[IntegerModulo, IntegerModulo, IntegerModulo] = {
      if (arg1 == 0) return (arg2, 0, 1)

      var a = arg1
      var b = arg2

      var x: IntegerModulo = 0
      var y: IntegerModulo = 0

      var x1: IntegerModulo = 0
      var y1: IntegerModulo = 0

      val res = gcdex (b%a, a)
      var d = res._1;
      x1 = res._2
      y1 = res._3

      x = y1 - (b / a) * x1;
      y = x1;

      return (d, x, y)
    }*/

    @tailrec def gcd[T : ExtendedRing](arg1: T, arg2: T) : T = {
      import ExtendedRing._
      if (arg2 |==| _zero_) arg1 else gcd(arg2, arg1 |%| arg2)
    }

    /*
     *  Returns tuple: (GCD, x, y) where x,y - results of extended Euclidean algorithm
     */
    def gcdex[T : ExtendedRing](arg1: T, arg2: T) : Tuple3[T, T, T] = {
      import ExtendedRing._
      import Ring._

      if (arg1 |==| _zero_) return (arg2, _zero_, _neutral_)  //TODO: this is bijection between integers and ring types vals, so it may be more accurate to define multiply and addition neutral elements

      var a = arg1
      var b = arg2

      var x: T = _zero_
      var y: T = _zero_

      var x1: T = _zero_
      var y1: T = _zero_

      val res = gcdex (b |%| a, a)
      var d = res._1;
      x1 = res._2
      y1 = res._3

      val t1 = b |/| a
      val t2 = t1 |*| x1
      x = y1 |-| t2      //x = y1 - (b /  a) * x1;
      y = x1;

      return (d, x, y)
    }
}
