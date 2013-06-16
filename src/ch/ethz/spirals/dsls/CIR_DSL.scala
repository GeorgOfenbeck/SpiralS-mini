/**
 *  SpiralS Mini - ETH Zurich
 *  Copyright (C) 2013  Alen Stojanov (astojanov@inf.ethz.ch)
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

import ch.ethz.spirals.rewrites._
import ch.ethz.spirals.dsls.cir._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.NestedBlockTraversal

trait CIR_DSL extends NumericOpsExpOpt with ArrayOpsExp with LiftNumeric with TrigonometryExp { self =>

  object optimizer extends CIROptimizer {
    val IR: self.type = self
  }

  object codegen extends CUnparser with CGenNumericOps with TrigonometryOps with CGenArrayOpsExt  {
    val IR: self.type = self
  }

  def funcToBlock[B](f: List[Exp[Any]] => Exp[B]) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val symList = mList.map(m => fresh(m))
    (symList, reifyEffects[B](f(symList)))
  }

  def opsCount[B](block:(List[Sym[Any]], Block[B])) : (Int, Int) = {
    val opsTraversal = new NestedBlockTraversal {
      val IR:self.type = self
      var add = 0
      var mul = 0
      override def traverseStm(stm: Stm): Unit = {
        stm match {
          case TP(_, NumericPlus(_, _))   => add += 1
          case TP(_, NumericMinus(_, _))  => add += 1
          case TP(_, NumericTimes(_, _))  => mul += 1
          case TP(_, NumericDivide(_, _)) => mul += 1
          case _ => // do nuttin
        }
        super.traverseStm(stm)
      }
    }
    opsTraversal.traverseBlock(block._2)
    (opsTraversal.add, opsTraversal.mul)
  }

}


















