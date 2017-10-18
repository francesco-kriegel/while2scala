package while2scala

trait WHILECompilationError
case class WHILELexerError(msg: String) extends WHILECompilationError
case class WHILEParserError(msg: String) extends WHILECompilationError
case class WHILEPreCompilerError(msg: String) extends WHILECompilationError