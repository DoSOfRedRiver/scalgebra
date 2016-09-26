package com.gmail.dosofredriver.math.scalaalgebra.matrix

import com.gmail.dosofredriver.math.scalaalgebra.core.structures.{ExtendedRing, Ring}
import com.gmail.dosofredriver.math.scalaalgebra.core.structures.Ring._

//TODO change with dependent types
class Matrix[T : Ring](val dim1: Int)(val dim2: Int) (val elements: T*) {
  require((dim1 * dim2) == elements.length, "Dimensions do not match to count of given arguments!")


  def + (matrix: Matrix[T]): Matrix[T] = {
    require(matrix.dim1 == dim1 && matrix.dim2 == dim2, "Dimensions of matrix do not match!")

    new Matrix(dim1)(dim2)((matrix.elements,elements).zipped.map(_ |+| _):_*)
  }

  def - (matrix: Matrix[T]): Matrix[T] = {
    require(matrix.dim1 == dim1 && matrix.dim2 == dim2, "Dimensions of matrix do not match!")

    new Matrix(dim1)(dim2)((elements, matrix.elements).zipped.map(_ |-| _):_*)
  }

  def * (matrix: Matrix[T]): Matrix[T] = {
    require(matrix.dim1 == dim2, "R and L dimensions of matrix do not match")

    val matr1 = getRows(this)                    //get rows of first matrix
    val matr2 = getCols(matrix)

    val res = for (a <- matr1; b <- matr2)
      yield (a, b).zipped.map(_ |*| _).reduce(_ |+| _)    //multiply matrix


    new Matrix(dim1)(matrix.dim2)(res.toList:_*)
  }


  /*def determinant : IntegerModulo = {
    gaussianElimination._1
  }


  def inverse : Matrix = {
    gaussianElimination._2
  }*/


  /*def gaussianElimination : Tuple2[IntegerModulo, Matrix] = {
    require(dim1 == dim2, "Is not square matrix!")

    val matrix = elements.grouped(dim1).map(x => x.toArray).toArray
    val trianMatr = Array.tabulate[IntegerModulo](dim1, dim1)((x,y) => if (x == y) 1 else 0)

    var accum = new IntegerModulo(1)

    //matrix's rows functions
    val multStr = (str: Array[IntegerModulo], mult: IntegerModulo) => str.map(x => x * mult)
    val subStr = (arg1: Array[IntegerModulo], arg2: Array[IntegerModulo], coef: IntegerModulo)
      => (arg1, arg2).zipped.map((x,y) => x - (y * coef))

    //first triangulation
    for (i <- 0 until dim1) {
      val a = matrix(i)(i).inverse

      matrix(i) = multStr(matrix(i), a)
      trianMatr(i) = multStr(trianMatr(i), a)

      accum = accum * a

      for (j <- (i+1) until dim1) {
        val coef = matrix(j)(i)

        matrix(j) = subStr(matrix(j), matrix(i), coef)
        trianMatr(j) = subStr(trianMatr(j), trianMatr(i), coef)

        accum = accum * coef
      }
    }

    if (accum == 0) return (accum, null)

    //second triangulation
    for (i <- (dim1-1) to 0 by -1) {
      for (j <- (i-1) to 0 by -1) {
        val coef = matrix (j)(i)

        matrix(j) = subStr(matrix(j), matrix(i), coef)
        trianMatr(j) = subStr(trianMatr(j), trianMatr(i), coef)
      }
    }

    (accum, new Matrix(dim1)(dim1)(trianMatr.flatten:_*))
  }*/


  //TODO move to another class
  private def getCols(matrix: Matrix[T]): IndexedSeq[IndexedSeq[T]] = {
    (for (ind <- 0 until matrix.dim2) //get cols of second matrix
      yield for (i <- 0 until matrix.elements.length if (i % matrix.dim2 == ind))
        yield matrix.elements(i))
  }

  //TODO move to another class
  private def getRows(matrix: Matrix[T]): Array[Seq[T]] = {
    matrix.elements.grouped(matrix.dim2).toArray
  }

  override def toString : String = {
    elements
      //.slice(0, dim1 * dim2)
      .map(x => f"$x%8s")
      .grouped(dim2)
      .map(x => x.mkString(" "))
      .mkString("\n")
      .mkString("")
  }
}

object Matrix {

  implicit object MatrixRing extends Ring[Matrix[Any]] {
    override def multiply(value: Matrix[Any], arg: Matrix[Any]): Matrix[Any] = value * arg

    override def minus(value: Matrix[Any], arg: Matrix[Any]): Matrix[Any] = value - arg

    override def plus(value: Matrix[Any], arg: Matrix[Any]): Matrix[Any] = value + arg
  }
}