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

import virtualization.lms._
import common._
import internal._

import scala.math._

import ch.ethz.spirals.dsls._


//-------------------------------------------------------------------------------
//Data Structures used to store the nodes of the RuleTree

//after 1st application:
// BreakDown ( DFT(x),
//               rule: DFT_CT
//               rule2spl: f
//               children: DFT(x1), DFT(x2)
//           )
//               where DFT(x1) and DFT(x2) look like
//
//            BreakDown( DFT(x1), None)

//The idea is that the Option Type is replace during the search (copy)

//The final node should like like this:
// BreakDown (DFT(2),
//              rule: DFT_Base
//              children: F2

//-----------------------------------------------------------------------------
//this is used once its decided which breakdown to use (rule/children)
class BreakDown_resolved (
                           val rule : PartialFunction[SPL,List[BreakDown]],
                           val children : List[BreakDown]
                           )
//this stores the non terminal and possibly the rule that is applied

class BreakDown(
                 val nt: SPL,
                 val applied_bd: Option[BreakDown_resolved]
                 )
{
  def this(nt: SPL) = this(nt,None)
  //=================================================================
  def PrintRuleTree(): Unit = {
    printrecurse(this,0)
    def printrecurse(in_bd: BreakDown, level: Int)
    {
      for (i <- 0 until level)
        print(" ")
      println(in_bd.nt)
      in_bd.applied_bd map (bd_applied => bd_applied.children map ( child => {printrecurse(child,level+1)} ))

    }
  }
}
//=================================================================================

trait BreakDown2SPL{
  val IR: SPL_Exp
  import IR._


  def bd2spl(in_bd: BreakDown): Rep[SPL] = {
    val r: Rep[SPL] = in_bd.applied_bd map (x =>
      x.rule match {
        case BreakdownRules.DFT_CT => {

          val k = in_bd.nt match {
            case DFT(n, k) => k
            case _ => 1
          }

          val k1 = x.children(0).nt.size //divisors(0)
          val d1 = x.children(1).nt.size //divisors(1)
          val n1 = k1 * d1
          //variable : Type                   = Parameter => body
          val extended_children = x.children map (child => bd2spl(child))
          val c1 = extended_children(0)
          val c2 = extended_children(1)

          val spl_expression = {
            //SPL - this is the actual SPL expression - c1 and c2 are the possibly extended children
            (c1 tensor I(d1)) compose T(n1, d1, k) compose (I(k1) tensor c2) compose L(n1, k1)
            //--------------------------------------------------------------------------------
          }
          spl_expression
        }
        case BreakdownRules.DFT_Base => {
          val spl: Rep[SPL] = F_2()
          spl
        }

        case _ =>
          assert(false, "Something is terribly wrong - ");
          val spl: Rep[SPL] = F_2()
          spl
      }
      ) getOrElse (
      {
        assert(false,"Something is terribly wrong - ");
        val spl: Rep[SPL] = F_2()
        spl
      }
      )
    r

  }
}


object BreakdownRules {
  import ch.ethz.spirals.util._

  // DFT_CT: 1965
  //   General Cooley-Tukey Rule
  //   DFT_n = (DFT_n/d tensor I_d) * diag * (I_n/d tensor F_d) * perm
  //
  // Cooley/Tukey:
  //   An Algorithm for the Machine Calculation of Complex Fourier Series.
  //   Mathematics of Computation, Vol. 19, 1965, pp. 297--301.


  val DFT_CT : PartialFunction[SPL,List[BreakDown]] =
  {
    case (DFT(n,k))
      if (n > 2 && !Utilities.IsPrimeInt(n)) => //this is the guard
    {
      //create Breakdown option with uninitialized children (no breakdown defined yet - only Non Terminal
      Utilities.DivisorPairs(n).map(pair=> new BreakDown(DFT(n,k),
                                     Some( new BreakDown_resolved(
                                              DFT_CT,
                                             List(
                                               new BreakDown(DFT(pair(0),Utilities.mathmod(k,pair(0)))),
                                               new BreakDown(DFT(pair(1),Utilities.mathmod(k,pair(1))))
                                             )))))
    }
  }

  //The final node should like like this:
  // BreakDown (DFT(2),
  //              rule: DFT_Base
  //              rule2spl: f (just take child)
  //              children: F2
  val DFT_Base : PartialFunction[SPL,List[BreakDown]] =
  {
    case (DFT(n,k))
      if (n == 2) => //this is the guard
    {
      List(new BreakDown(DFT(n,k), Some( new BreakDown_resolved(
                              DFT_Base,
                              //f,
                              List(new BreakDown(F_2()))
                                ))
      ))
    }
  }


  val all = List(DFT_CT, DFT_Base)





}