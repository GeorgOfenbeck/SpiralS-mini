/**
 * Georg Ofenbeck
 First created:
 * Date: 10/06/13
 * Time: 22:05 
 */
import ch.ethz.spirals.search.Search._
import ch.ethz.spirals.dsls._
import ch.ethz.spirals.util.Utilities._
import ch.ethz.spirals.rewrites._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._


//DFT4 and all the components we need to reconstruct it
val dft4 = DFT(4,1)
val dft2 = DFT(2,1)
val f2 =  F_2()
val twiddle = T(4,2,1)
val permuation = L(4,2)
val identity = I(2)

val blocked = kronecker(identity.toMatrix,f2.toMatrix)
val strided = kronecker(f2.toMatrix,identity.toMatrix)
val all_together = strided.multiply(twiddle.toMatrix.multiply(blocked.multiply(permuation.toMatrix)))

//first lets have a look into DFT4
println
println("DFT4 by definition")
dft4.pM
println
println("DFT2 by definition")
dft2.pM
println
println("We use F_2 as symbol for DFT2 (that is just to make more clear that its not getting decomposed further")
f2.pM
println
println("The permutation:")
permuation.pM
println
println("I2 tensor F2")
printm(blocked)
println
println("The twiddle scaling")
twiddle.pM
println
println("F2 tensor I2")
printm(strided)
println
println("The composition multiplied together")
printm(all_together)

import org.apache.commons.math3.linear._
import org.apache.commons.math3.complex.{ComplexFormat, ComplexField, Complex}
import org.apache.commons.math3.transform.{TransformType, DftNormalization, FastFourierTransformer}
val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(),4,1)

for (i <- 0 until 4)
  m.setEntry(i,0,new Complex(i,0))

val result = all_together.multiply(m)
println("Result of DFT")
printm(result)

