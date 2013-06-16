/**
 * Georg Ofenbeck
 First created:
 * Date: 10/06/13
 * Time: 22:35 
 */

import ch.ethz.spirals.search.Search._
import ch.ethz.spirals.dsls._
import ch.ethz.spirals.util.ExportGraphCIR
import ch.ethz.spirals.util.Utilities._
import ch.ethz.spirals.rewrites._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import java.io.PrintWriter


val dftsize = 64
val spl_input = DFT(dftsize,1)
for (i<- 0 until 5)
{
  println("================================================")
  println("Breakdown "+ i)
  val breakdown = ch.ethz.spirals.search.Search.RandomRuleTree(spl_input)
  breakdown.PrintRuleTree()
}


