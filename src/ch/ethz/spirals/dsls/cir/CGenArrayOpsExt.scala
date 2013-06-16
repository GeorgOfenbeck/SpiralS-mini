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

package ch.ethz.spirals.dsls.cir

import ch.ethz.spirals.rewrites.CUnparser

import scala.virtualization.lms.common.CGenBase
import scala.virtualization.lms.common.CLikeGenArrayOps
import scala.virtualization.lms.common.ArrayOpsExp

trait CGenArrayOpsExt extends CGenBase with CLikeGenArrayOps with CUnparser { self =>

  val IR: ArrayOpsExp
  import IR._

  var mallocArraySyms = List.empty[Sym[Any]]

  def setMallocArrays(syms: List[Sym[Any]]) = {
    mallocArraySyms = syms
    includeHeaders = includeHeaders ::: List("stdlib.h")
  }

  def checkForReturnArray(block: Block[Any]) = {
    mallocArraySyms = List.empty[Sym[Any]]
    findDefinition(getBlockResultFull(block).asInstanceOf[Sym[Any]]) match {
      case Some(tp) => tp match {
        case TP(_, Reify(s, _, _)) if (s.isInstanceOf[Sym[_]]) => findDefinition(s.asInstanceOf[Sym[Any]]) match {
          case Some(stm) => stm match {
            case TP(sym, Reflect(ArrayNew(_), _, _)) => setMallocArrays(List(sym.asInstanceOf[Sym[Any]]))
            case TP(sym, ArrayNew(_)) => setMallocArrays(List(sym.asInstanceOf[Sym[Any]]))
            case _ =>
          }
          case None =>
        }
      }
      case None =>
    }
  }

  override def checkReturnValue(block: Block[Any]) = {
    super.checkReturnValue(block)
    //checkForReturnArray(block)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ArrayLength(x) => emitValDef(sym, quote(x) + ".length")
      case ArrayApply(x,n) => emitValDef(sym, quote(x) + "[" + quote(n) + "]")
      case ArrayUpdate(x,n,y) => stream.println(quote(x) + "[" + quote(n) + "] = " + quote(y) + ";")
      case ArrayNew(n) => if ( mallocArraySyms.contains(sym) ) {
        stream.println(remap(sym.tp) + " " + quote(sym) + " = (" + remap(sym.tp) +") malloc (sizeof(" + getArrayInnerTypeManifest(sym.tp) + ") * " + quote(n) + ");")
      } else {
        stream.println(getArrayInnerTypeManifest(sym.tp) + " " + quote(sym) + "[" + quote(n) + "];")
      }
      case Reify(s, _, _) => if ( validReturns.contains(s) ) {
        stream.println("return " + quote(s) + ";")
      } else {
        // stream.println("//return " + quote(s) + ";")
      }
      case _ => super.emitNode(sym, rhs)
    }
  }
}