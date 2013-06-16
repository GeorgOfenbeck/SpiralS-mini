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

/**
 * SPL as a staged DSL
 */
trait SPL_Base extends Base  {

  //=============================================================================
  // SPL Operators
  //=============================================================================
  implicit def SPLtoRep (i: SPL): Rep[SPL] = unit(i)
  //implicit def rReptoRep (i: rSPL): Rep[rSPL] = unit(i)


  implicit def mkSPLRepOps(lhs: Rep[SPL]): SPLOps = new SPLOps(lhs)
  class SPLOps (x: Rep[SPL]) {
    def tensor(y: Rep[SPL]) = infix_tensor(x,y)
    def compose(y: Rep[SPL]) = infix_compose(x,y)
  }
  def infix_tensor(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]
  def infix_compose(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]
}



trait SPL_Exp extends SPL_Base with BaseExp  {

  case class Tensor(x: Exp[SPL], y: Exp[SPL])  extends Def[SPL]
  case class Compose(x: Exp[SPL], y: Exp[SPL]) extends Def[SPL]

  def infix_tensor(x: Exp[SPL], y: Exp[SPL]) =  Tensor(x,y)
  def infix_compose(x: Exp[SPL], y: Exp[SPL]) = Compose(x,y)
}


class SPL_DSL extends SPL_Exp





