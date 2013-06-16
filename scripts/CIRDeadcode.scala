import ch.ethz.spirals.dsls.CIR

println("Simlify the graph")
var block = (List(fresh_var), cirBlock)
var blockOptimization_1 = CIR.optimizer.elimZeroArithmetics(block)

CIR.codegen.emitTransformedBlock(blockOptimization_1, "dft", new PrintWriter(System.out))

println("Optimizations before: " + CIR.opsCount(block))
println("Optimizations after : " + CIR.opsCount(blockOptimization_1))