import ch.ethz.spirals.search.Search._
import ch.ethz.spirals.dsls._
import ch.ethz.spirals.util.Utilities._
import ch.ethz.spirals.rewrites._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

println("Lets now work with a staged DSL")
val dsl = new SPL_DSL
import dsl._
val breakdown = (F_2() tensor I(2) ) compose T(4,2,1) compose (I(2) tensor F_2()) compose L(4,2)

val operations_using_DSL = new ExportGraph with SPL2Mat with SPL2Scala{
  val IR: dsl.type = dsl
}

operations_using_DSL.emitDepGraph(breakdown,"DFT_SPL.graph")
val matrices = operations_using_DSL.SPL2Mat(breakdown);
println
println("Composed matrices from the DSL")
printm(matrices.last)

println
println("Now we could unparse naivly such as:")
L(4,2).naiveUnparse()

println("or create a Scala function like this:")
val f_array = operations_using_DSL.SPL2Scala(breakdown)


println("testing it out")
val example_input = new Array[AComplex](4)
for (i <- 0 until 4) example_input(i) = new AComplex(i.asInstanceOf[Double], 0.0)
val result = f_array.last(example_input)


println("Printing the result of calling scala")
result map println




