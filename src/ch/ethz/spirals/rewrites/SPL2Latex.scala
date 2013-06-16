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
trait SPL2Latex extends GraphTraversal{
  import ch.ethz.spirals.dsls._

  val IR: SPL_Exp

  import IR._
  import scala.collection.mutable.HashMap
  import org.apache.commons.math3.linear.BlockFieldMatrix
  import org.apache.commons.math3.complex.{ComplexField, Complex}

  //-----------------------------------------Matrix Representation Part --------------------------------
  def SPL2Latex (start: Exp[Any]) = {
    val deflist = buildScheduleForResult(start)
    val formulars = new Array[String](deflist.size)
    val index_array = new HashMap[Int,Int]
    var i : Int = 0
    for (TP(sym, rhs) <- deflist) {
      val index = sym match {
        case Sym(n) => n
        case _ => -1
      }
      index_array += (index -> i)
      formulars(index) = emitFormular(sym, rhs,formulars , index_array)
      i = i + 1
    }
    formulars
  }

  def emitFormular(sym: Sym[Any], rhs: Def[Any], formulars: Array[String],  lt : HashMap[Int,Int]): String = rhs match {
    //--------------------------------Compose -----------------------------
    case Compose(a1, b1) => {
      (a1, b1) match {
        case (Sym(a), Sym(b)) => formulars(lt(a)) + formulars(lt(b))
         case (Sym(a), Const(b: SPL)) => formulars(lt(a)) + b.toLatex()
         case (Const(b: SPL), Sym(a)) => b.toLatex() + formulars(lt(a))
         case (Const(a: SPL), Const(b: SPL)) => a.toLatex() + b.toLatex()
      }
    }
    //-------------------------------Tensor--------------------------------
    case Tensor(a1, b1) => {
      (a1, b1) match {
        case (Sym(a), Sym(b)) =>               "(" + formulars(lt(a)) + "\\tensor" + formulars(lt(b)) + ")"
         case (Sym(a), Const(b: SPL)) =>        "(" + formulars(lt(a)) + "\\tensor" + b.toLatex()  + ")"
         case (Const(b: SPL), Sym(a)) =>        "(" +  b.toLatex() + "\\tensor" + formulars(lt(a)) + ")"
         case (Const(a: SPL), Const(b: SPL)) => "(" +  a.toLatex() + "\\tensor" + b.toLatex()  + ")"
      }
    }
  }



}

