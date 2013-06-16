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

package ch.ethz.spirals.rewrites

import scala.virtualization.lms.common._
import virtualization.lms.internal._

import java.io._

trait CUnparser extends CCodegen  {

  val IR: Expressions with EffectExp
  import IR._

  var includeHeaders = List.empty[String]
  var validReturns = List.empty[Sym[Any]]

  def getArrayInnerTypeManifest[A](m: Manifest[A]) = {
    Manifest.classType(m.erasure.getComponentType)
  }

  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[Any]] ) {
      remap(m.typeArguments.head)
    }
    else {
      if (m.erasure.isArray()) {
        val mp = getArrayInnerTypeManifest(m)
        remap(mp) + "*"
      } else {
        m.toString match {
          case "double" => "double"
          case "float" => "float"
          case "int" => "int"
          case _ => super.remap(m)
        }
      }
    }
  }

  def checkReturnValue(block: Block[Any]) = {
    validReturns = List.empty[Sym[Any]]
  }

  def writeIncludeHeaders (out: PrintWriter) = {
    for ( header <- includeHeaders ) {
      out.println("#include <" + header + ">")
    }
  }

  def emitTransformedBlock[B](block:(List[Sym[Any]], Block[B]), functionName: String, out: PrintWriter) (implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val stringOutput = new StringWriter()
    val stringWriter = new PrintWriter(stringOutput)
    validReturns = List.empty[Sym[Any]]
    val (x, y) = (block._1, block._2)
    val staticData = getFreeDataBlock(y)
    checkReturnValue(y)
    writeIncludeHeaders(stringWriter)
    withStream(stringWriter) {
      stream.println(remap(mB) + " " + functionName + "(" + (mList zip x).map(m => remap(m._1) + quote(m._2)).mkString(", ") + ") {")
      emitBlock(y)
      stream.println("}")
    }
    stringWriter.flush()

      out.write(stringOutput.toString);

    out.flush()
    staticData
  }

  def emitSource[B](f: List[Exp[Any]] => Exp[B], functionName: String, out: PrintWriter)(implicit mList: List[Manifest[Any]], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val p = mList.map(m => fresh(m))
    val q = reifyBlock[B](f(p))
    emitTransformedBlock((p, q), functionName, out)
  }

  override def quote(x: Exp[Any]) : String = x match {
    case s@Sym(n) if (s.tp.toString() == "Int") => "i"+n
    case _ => super.quote(x)
  }
}
