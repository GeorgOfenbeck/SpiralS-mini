/**
 * Created with IntelliJ IDEA.
 * User: Georg
 * Date: 15/06/13
 * Time: 16:55
 * To change this template use File | Settings | File Templates.
 */

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
val breakdown = ch.ethz.spirals.search.Search.RandomRuleTree(spl_input)
breakdown.PrintRuleTree()


println("Translating Breakdown to SPL")
val dsl = new SPL_DSL
val spltransformation= new BreakDown2SPL with ExportGraph with SPL2Mat with SPL2Scala{
  val IR: SPL_Exp = dsl
}
val spl = spltransformation.bd2spl(breakdown)


println("creating again a function- but this time staged")
val f_array = spltransformation.SPL2Scala(spl)


println("Putting a header and trailer around it")

val size = dftsize
val fft : (CIR.Rep[Array[Double]] => CIR.Rep[Array[Double]]) = (input: CIR.Rep[Array[Double]]) =>
{
  {
    import CIR._
    val tmp_array = new Array[AComplex](size)
    for (i <- 0 until (size) )
    {
      val re = input(i*2)
      val im = input(i*2+1)
      tmp_array(i) = new AComplex(re,im)
    }
    val res = f_array.last(tmp_array)


    val t_out = NewArray[Double](size*2)

    for (i <- 0 until (size) ) {
      t_out(i*2) = res(i)._re
      t_out(i*2+1) =res(i)._im
    }

    t_out
  }
}

val fresh_var = CIR.fresh[Array[Double]]
val cirBlock = CIR.reifyEffects(fft(fresh_var))

println("Printing the CIR graph")
val graphExport = new ExportGraphCIR {
  val IR: CIR.type = CIR
  disableArrayUpdateDeps = true
}
graphExport.exportGraph((fresh_var, cirBlock), "cir_" + dftsize + " .dot", true)

println("Emitting the C code")
implicit val mList = List(manifest[Array[Double]]).asInstanceOf[List[Manifest[Any]]]
CIR.codegen.emitTransformedBlock((List(fresh_var), cirBlock), "dft", new PrintWriter(System.out))



