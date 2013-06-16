import ch.ethz.spirals.search.Search._
import ch.ethz.spirals.dsls._
import ch.ethz.spirals.util.ExportGraphCIR
import ch.ethz.spirals.util.Utilities._
import ch.ethz.spirals.rewrites._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import java.io.PrintWriter

println("Lets now work with a staged DSL")
val dsl = new SPL_DSL
import dsl._
val breakdown = (F_2() tensor I(2) ) compose T(4,2,1) compose (I(2) tensor F_2()) compose L(4,2)

val operations_using_DSL = new ExportGraph with SPL2Scala{
  val IR: dsl.type = dsl
}

println("creating again a function- but this time staged")
val f_array = operations_using_DSL.SPL2Scala(breakdown)


println("Putting a header and trailer around it")

val size = 4
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
graphExport.exportGraph((fresh_var, cirBlock), "cir.dot", true)

println("Emitting the C code")
implicit val mList = List(manifest[Array[Double]]).asInstanceOf[List[Manifest[Any]]]
CIR.codegen.emitTransformedBlock((List(fresh_var), cirBlock), "dft", new PrintWriter(System.out))