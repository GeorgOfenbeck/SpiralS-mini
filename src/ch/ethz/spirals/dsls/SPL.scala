/**
 *  SpiralS Mini - ETH Zurich
 *  Copyright (C) 2013 Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see http://www.gnu.org/licenses/.
 */

package ch.ethz.spirals.dsls

import virtualization.lms.common._
import reflect.SourceContext

import scala.math._
import org.apache.commons.math3.linear._
import org.apache.commons.math3.complex.{ComplexFormat, ComplexField, Complex}
import org.apache.commons.math3.transform.{TransformType, DftNormalization, FastFourierTransformer}





/*object CIR extends CIR_DSL
case class AComplex(_re: CIR.Rep[Double], _im: CIR.Rep[Double]){
  import CIR._*/
case class AComplex(_re: Double, _im: Double){
  def +(that: AComplex) = AComplex(this._re + that._re, this._im + that._im)
  def -(that: AComplex) = AComplex(this._re - that._re, this._im - that._im)
  def *(that: AComplex) = AComplex(this._re * that._re - this._im * that._im, this._re * that._im + this._im * that._re)
}




/**
 * SPL is the base type we operate on which is a always a special matrix in the context of e.g. DFTs such as
 * Stride permutation etc.
 * @param size
 */

abstract class SPL(val size: Int) {
  //SPL Specific part
  def transform(in: Array[AComplex]): Array[AComplex];
  def toLatex(): String = "No Latex defined"
  def toMatrix() : BlockFieldMatrix[Complex] =
  {
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(),size,size)
      for (i <- 0 until size)
      {
        val x = new Array[AComplex](size)
        for (j <- 0 until size)
          if (j == i)
            x.update(i,new AComplex(1.0,0.0))
          else
            x.update(j,new AComplex(0.0,0.0))

        val outputrow = transform(x)

        for (j <- 0 until size)
        {
          val re: Double = outputrow.apply(j)._re
          val im: Double = outputrow.apply(j)._im
          m.setEntry(j,i,new Complex(re,im))
        }
      }
    m
  }
  def pM() = ch.ethz.spirals.util.Utilities.printm(toMatrix())
}

/**
 * Discrete Fourier Transform
 * @param n size
 * @param k
 */
case class DFT(n: Int, k: Int) extends SPL(n)
{
  def this(n: Int) = this(n,1)
  override def toString = "DFT("+n+","+k+")"
  override def toLatex = if (k != 1) "\\DFT("+n+","+k+")" else "\\DFT("+n+")"
  override def toMatrix() = {
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(),n,n)
    val t_e = ch.ethz.spirals.dsls.E(n)
    for (x <- 0 until n)
      for (y <- 0 until n)
        m.setEntry(x,y,new Complex(t_e.re(x*y*k),t_e.im(x*y*k)))
    m
  }
  //TODO: add by definition transform
  override def transform(in: Array[AComplex]): Array[AComplex] = { assert(false, "We never want to transform an input with the DFT by Definition"); in}
}

/**
 * This is the basic building block ("the butterfly") of all DFT's - note the fixed size 2
 */
case class F_2() extends SPL(2){
  override def toString = "F2"
  override def toLatex = "\\DFT_{2}"
  override def transform( in: Array[AComplex]): Array[AComplex] =
  {
    val out = new Array[AComplex](in.size)
    val t1 = in(0)
    val t2 = in(1)
    val r1 = t1 + t2
    val r2 = t1 - t2
    out.update(0,r1)
    out.update(1,r2)
    out
  }
}


/**
 * Describes properties that all StridePermutations such as "L" share
 */
trait StridePermutation extends SPL{
  //def transform[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = permute(in)
  //def reorder[V[_], E[_], T](in: CVector[V, E, T], order: List[Int])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T] = in;
  //def permute[V[_], E[_], T](in: CVector[V, E, T])(implicit vrep: VRepType[V], erep: ERepType[E],  nrep: NumericRep[T]): CVector[V, E, T];
  def transpose (order: List[Int]): List[Int] =
  {
    val t_array = new Array[Int](order.size)
    for (i <- 0 until order.size) {
      val pos = order(i)
      t_array(pos) = i }
    t_array.toList
  }
}





/**
 * Identity matrix - usually only used in SPL for tensors and gone during SigmaSPL
 * @param n size
 */
case class I(n: Int) extends SPL(n) with StridePermutation
{
  override def toString = "I("+n+")"
  override def toLatex = "\\I_{"+n+"}"
  override def transform( in: Array[AComplex]): Array[AComplex] = in
}


/**
 * Twiddle Scaling matrix
 * @param n size
 * @param d TODO
 * @param k TODO
 */
case class T(n: Int,d: Int,k : Int) extends SPL(n)
{
  def this(n: Int, d: Int) = this(n,d,1)
  import ch.ethz.spirals.util._
  override def toString = "T("+ n + ")("+ d +")("+ k +")"
  override def toLatex = "\\T^{"+n+"}_{"+d+"}"
  override def transform( in: Array[AComplex]): Array[AComplex] =
  {
    val out = new Array[AComplex](in.size)
    val diag  = Utilities.diagTensor(Utilities.dLin(n/d,1,0), Utilities.dLin(d,1,0))
    val t = E(n)
    val root_list_re = diag map ( ele => t.re(ele.toInt*k))
    val root_list_im = diag map ( ele => t.im(ele.toInt*k))
    for (i <- 0 until root_list_re.size)
    {
      val t = AComplex(root_list_re(i),root_list_im(i))
      val res = in(i) * t
      out.update(i,res)
    }
    out
  }
}

/**
 * TODO
 * @param n
 * @param k
 */
case class L(n: Int,k: Int) extends SPL(n) with StridePermutation
{
  override def toString = "L("+ n + ")("+ k +")"
  override def toLatex = "\\L^{"+n+"}_{"+k+"}"
  override def transform( in: Array[AComplex]): Array[AComplex] =
  {
    val out = new Array[AComplex](in.size)
    for (i <- 0 until n)
    {
      val m = n/k
      val newindex = i/m + k* (i%m)
      out(i) = in(newindex)
    }
    out
  }

  def naiveUnparse() =
  {
    println("//just to sketch how we could unparse the DSL naivly")
    for (i <- 0 until n)
    {
      val m = n/k
      val newindex = i/m + k* (i%m)
      //out(i) = in(newindex)
      println("outarray["+i+"] = inarray["+ newindex+ "];")
    }
    println("..... and so forth....")

  }

}




//This represents a root of Unity
case class E(val n : Int){
  private def yieldk(n:Int) = {
    def tmp () = {
      for(k <- 0 until n
          if ( //this if checks if x^t becomes 1 before n==t, this is e.g. the case for 2nd root of unity of 4 where it becomes 1 at x^2
            (for (t <- 2 until n-1
                  if (Math.cos(2*math.Pi*k*t/n) == 1)
            )yield 1
              ).isEmpty
            )
      )
      yield k
    }
    tmp.last
  }
  val store =yieldk(n)
  def re(p: Int): Double = CosPi(2.0 * p * store, n)
  def im(p: Int): Double = SinPi(2.0 * p * store, n) * -1.0


  def valueSinOrCos (f: String, x: Double, y: Double): Double = {
    import scala.math._
    val (sign, trig, xn, yn, value) = ch.ethz.spirals.util.Utilities.normalize_trig (1, f, x, y)
    if (! value.equals (Double.MaxValue) ) {
      value
    } else {
      trig match {
        case "sin" => (xn, yn) match {
          case (1.0, 6.0) => sign * 0.5
          case _ => sign * sin (xn * math.Pi / yn)
        }
        case "cos" =>sign * cos (xn * math.Pi / yn)

      }
    }
  }

  def SinPi (x: Double, y: Double): Double = {
    valueSinOrCos ("sin", x, y)
  }

  def CosPi (x: Double, y: Double): Double = {
    valueSinOrCos ("cos", x, y)
  }
  //def re(p: Int): Double = Math.cos(2.0 * p * store/ n * Math.PI)
  //def im(p: Int): Double = Math.sin(2.0 * p * store/ n * Math.PI) * -1.0
}




