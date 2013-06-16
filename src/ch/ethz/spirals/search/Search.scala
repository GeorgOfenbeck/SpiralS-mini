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

package ch.ethz.spirals.search

import ch.ethz.spirals.rewrites._
import ch.ethz.spirals.dsls._

/**
 * This object collects all available search engines
 */
object Search {

  def RandomRuleTree(in_spl: SPL): BreakDown =
  {
    def random_breakdown(in_spl: SPL) : BreakDown =
    {
      val rnd = new scala.util.Random()
      val applicable = BreakdownRules.all filter (_.isDefinedAt(in_spl))
      if (applicable.size < 1) {
        new BreakDown(in_spl,None)
      }
      else
      {
        val rule_choice = scala.math.abs(rnd.nextInt()) % applicable.size //pick a random rule - in this case not really a choice
        val possible_breakdowns = applicable(rule_choice).apply(in_spl)
        val df_choice = scala.math.abs(rnd.nextInt()) % possible_breakdowns.size //pick a random breakdown
        val bd = possible_breakdowns(df_choice)
        val expanded_children = bd.applied_bd map ( bd_existing => bd_existing.children map (child =>  random_breakdown(child.nt)
          )) getOrElse (List())
        new BreakDown(bd.nt,Some(new BreakDown_resolved(bd.applied_bd.get.rule,expanded_children)))
      }
    }
    random_breakdown(in_spl)

  }
}






