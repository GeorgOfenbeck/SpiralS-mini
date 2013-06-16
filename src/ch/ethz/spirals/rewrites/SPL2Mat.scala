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

trait SPL2Mat extends GraphTraversal{
  import ch.ethz.spirals.dsls._

  val IR: SPL_Exp

  import IR._
  import scala.collection.mutable.HashMap
  import org.apache.commons.math3.linear.BlockFieldMatrix
  import org.apache.commons.math3.complex.{ComplexField, Complex}

  //-----------------------------------------Matrix Representation Part --------------------------------
  def SPL2Mat (start: Exp[Any]) = {
    val deflist = buildScheduleForResult(start)
    val f_array = new Array[ BlockFieldMatrix[Complex] ](deflist.size)
    val index_array = new HashMap[Int,Int]
    var i : Int = 0
    for (TP(sym, rhs) <- deflist) {
      val index = sym match {
        case Sym(n) => n
        case _ => -1
      }
      index_array += (index -> i)
      f_array(i) = matrix_emitNode(sym, rhs,f_array , index_array)
      i = i + 1
    }
    f_array
  }



  def matrix_emitNode(sym: Sym[Any], rhs: Def[Any],f_array: Array[ BlockFieldMatrix[Complex]] , lt : HashMap[Int,Int]):
  // returns matrix
  (  BlockFieldMatrix[Complex] ) =
  {
    import ch.ethz.spirals.util.Utilities._
    rhs match {

    //--------------------------------Compose -----------------------------
    case Compose(Sym(a),Sym(b)) => f_array(lt(a)).multiply( f_array(lt(b)) )
    case Compose(Sym(a),Const(x: SPL)) => f_array(lt(a)).multiply( x.toMatrix() )
    case Compose(Const(x: SPL), Sym(b)) =>  x.toMatrix().multiply( f_array(lt(b)) )
    case Compose(Const(x: SPL), Const(y: SPL)) =>  x.toMatrix().multiply( y.toMatrix() )
    //-------------------------------Tensor--------------------------------
    case Tensor(Sym(a),Sym(b)) => kronecker(f_array(lt(a)),f_array(lt(b)))
    case Tensor(Sym(a),Const(x: SPL)) => kronecker(f_array(lt(a)), x.toMatrix())
    case Tensor(Const(x: SPL), Sym(b)) =>  kronecker(x.toMatrix(),f_array(lt(b)))
    case Tensor(Const(x: SPL), Const(y: SPL)) =>  kronecker(x.toMatrix(),y.toMatrix())
  }

  }

}

