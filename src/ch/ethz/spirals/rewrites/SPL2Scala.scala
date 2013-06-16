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

package ch.ethz.spirals.rewrites

import scala.virtualization.lms._
import internal._

trait SPL2Scala extends GraphTraversal{
  import ch.ethz.spirals.dsls._

  val IR: SPL_Exp

  import IR._
  import scala.collection.mutable.HashMap
  import org.apache.commons.math3.complex.{ComplexField, Complex}

  //-----------------------------------------Matrix Representation Part --------------------------------
  def SPL2Scala (start: Exp[Any]) = {
    val deflist = buildScheduleForResult(start)
    val f_array = new Array[ Array[AComplex] => Array[AComplex] ](deflist.size)
    val index_array = new HashMap[Int,Int]
    var i : Int = 0
    for (TP(sym, rhs) <- deflist) {
      val index = sym match {
        case Sym(n) => n
        case _ => -1
      }
      index_array += (index -> i)
      f_array(i) = emitfunction(sym, rhs,f_array , index_array)
      i = i + 1
    }
    f_array
  }



  def emitfunction(sym: Sym[Any], rhs: Def[Any],f_array: Array[Array[AComplex] => Array[AComplex]] , lt : HashMap[Int,Int]):
  // returns matrix
  (Array[AComplex] => Array[AComplex] )=
  {
    import ch.ethz.spirals.util.Utilities._
    rhs match {

      //--------------------------------Compose -----------------------------
      case Compose(Sym(a),Sym(b)) => { (in: Array[AComplex]) => (f_array(lt(a)) ( f_array(lt(b)) (in) )) }
      case Compose(Sym(a),Const(x: SPL)) => { (in: Array[AComplex]) => f_array(lt(a))( x.transform(in) )}
      case Compose(Const(x: SPL), Sym(b)) =>  { (in: Array[AComplex]) =>x.transform( f_array(lt(b))(in) )}
      case Compose(Const(x: SPL), Const(y: SPL)) =>  { (in: Array[AComplex]) => x.transform( y.transform(in) ) }
      //-------------------------------Tensor--------------------------------
      case Tensor(Const(x: I), Sym(b)) =>  { (in: Array[AComplex]) => I_tensor_A(in,in.size,x.size,f_array(lt(b))) }
      case Tensor(Const(x: I), Const(y: SPL)) => { (in: Array[AComplex]) => I_tensor_A(in,in.size,x.size,y.transform) }
      case Tensor(Sym(a)       ,Const(x: I)) => { (in: Array[AComplex]) => A_tensor_I(in,in.size,x.size,f_array(lt(a))) }
      case Tensor(Const(a: SPL),Const(x: I)) => { (in: Array[AComplex]) => A_tensor_I(in,in.size,x.size,a.transform) }


    }

  }


  def I_tensor_A (in: Array[AComplex],in_size: Int, n: Int, A: ((Array[AComplex]) => Array[AComplex])): Array[AComplex] = {
    in.grouped(in_size/n).toArray flatMap (part => A(part))
    //split n into (in_size/n) parts - pass each part to A and return concatenated list
  }


  def A_tensor_I (in: Array[AComplex], in_size: Int, n: Int, A: ((Array[AComplex]) => Array[AComplex])): Array[AComplex] ={
    (in.grouped(n).toArray.transpose map (part => A(part)) transpose).flatten
    //take input - split it into parts of size/n read at stride (the transpose will do this) and apply A
  }

}