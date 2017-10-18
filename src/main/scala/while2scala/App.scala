package while2scala

/**
 * @author Francesco Kriegel
 */
object App {

  def main(args: Array[String]) {
    val source = scala.io.Source.fromFile(args(0))
    val sourceCode: String = try source.mkString finally source.close()
    println("source code:")
    println(sourceCode)
    println()
    val precompilationResult = WHILEPreCompiler(sourceCode)
    if (precompilationResult.isRight) {
      val precompiledCode = precompilationResult.right.get
      println("pre-compiled code:")
      println(precompiledCode)
      println()
      val compilationResult = WHILECompiler(precompiledCode)
      if (compilationResult.isRight) {
        val program = compilationResult.right.get
        println("compiled program:")
        println(program)
        println()
        val initialState = new WHILEState(args.tail.map(_.toLong))
        println("initial state:")
        println(initialState)
        println()
        val result = program(initialState)
        println("result:")
        println(result)
        println()
      } else
        println(compilationResult.left.get)
    } else
      println(precompilationResult.left.get)
  }

}