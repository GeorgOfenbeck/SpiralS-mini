import ch.ethz.spirals.dsls.CIR

println("Make all constants positive")
var blockOptimization_2 = CIR.optimizer.makeConstantsPositive(blockOptimization_1)
CIR.codegen.emitTransformedBlock(blockOptimization_2, "dft", new PrintWriter(System.out))

println("Optimizations before: " + CIR.opsCount(blockOptimization_1))
println("Optimizations after : " + CIR.opsCount(blockOptimization_2))