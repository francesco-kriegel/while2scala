package while2scala

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Position
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Reader

sealed trait WHILEToken
case class VARIABLE(key: Int) extends WHILEToken
case class LONG(value: Long) extends WHILEToken
case object NOTZERO extends WHILEToken
case object COLONEQQ extends WHILEToken
case object PLUS_ extends WHILEToken
case object MINUS_ extends WHILEToken
case object SEMICOLON extends WHILEToken
case object LOOP_ extends WHILEToken
case object WHILE_ extends WHILEToken
case object DO extends WHILEToken
case object END extends WHILEToken

object WHILELexer extends RegexParsers {

  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f\n]+".r

  def variable: Parser[VARIABLE] = "x[0-9]+".r ^^ { name ⇒ VARIABLE(name.substring(1).toInt) }
  def long: Parser[LONG] = "[0-9]+".r ^^ { string ⇒ LONG(string.toLong) }
  def notzero = "!=" ~ whiteSpace.? ~ "0" ^^ (_ => NOTZERO)
  def coloneqq = ":=" ^^ (_ ⇒ COLONEQQ)
  def plus = "+" ^^ (_ ⇒ PLUS_)
  def minus = "-" ^^ (_ ⇒ MINUS_)
  def semicolon = ";" ^^ (_ ⇒ SEMICOLON)
  def loop = "LOOP" ^^ (_ ⇒ LOOP_)
  def while_ = "WHILE" ^^ (_ ⇒ WHILE_)
  def do_ = "DO" ^^ (_ ⇒ DO)
  def end = "END" ^^ (_ ⇒ END)

  def tokens: Parser[List[WHILEToken]] = {
    phrase(rep1(variable | long | notzero | coloneqq | plus | minus | semicolon | loop | while_ | do_ | end))
  }

  def apply(code: String): Either[WHILELexerError, List[WHILEToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next)  ⇒ Left(WHILELexerError(msg + " at line " + next.pos.line + ", column " + next.pos.column + "\r\n" + next.pos.longString))
      case Success(result, next) ⇒ Right(result)
    }
  }

}

object WHILEParser extends Parsers {

  override type Elem = WHILEToken

  class WHILETokenReader(tokens: Seq[WHILEToken]) extends Reader[WHILEToken] {
    override def first: WHILEToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[WHILEToken] = new WHILETokenReader(tokens.tail)
  }

  private def variable: Parser[VARIABLE] = {
    accept("variable", { case variable @ VARIABLE(name) ⇒ variable })
  }

  private def long: Parser[LONG] = {
    accept("integer", { case integer @ LONG(value) ⇒ integer })
  }

  def plus: Parser[Plus] = {
    (variable ~ COLONEQQ ~ variable ~ PLUS_ ~ long) ^^ { case x ~ _ ~ y ~ _ ~ n ⇒ Plus(x.key, y.key, n.value) }
  }

  def minus: Parser[Minus] = {
    (variable ~ COLONEQQ ~ variable ~ MINUS_ ~ long) ^^ { case x ~ _ ~ y ~ _ ~ n ⇒ Minus(x.key, y.key, n.value) }
  }

  def composition: Parser[WHILEProgram] = {
    ((plus | minus | loop | while_) ~ rep1(SEMICOLON ~> (plus | minus | loop | while_))) ^^ {
      case p ~ ps ⇒ ps.foldLeft(p) {
        case (q, r) ⇒ Composition(q, r)
      }
    }
  }

  def loop: Parser[Loop] = {
    (LOOP_ ~ variable ~ DO ~ (composition | plus | minus | loop | while_) ~ END) ^^ { case _ ~ x ~ _ ~ p ~ _ ⇒ Loop(x.key, p) }
  }

  def while_ : Parser[While] = {
    (WHILE_ ~ variable ~ NOTZERO ~ DO ~ (composition | plus | minus | loop | while_) ~ END) ^^ { case _ ~ x ~ _ ~ _ ~ p ~ _ ⇒ While(x.key, p) }
  }

  def apply(tokens: Seq[WHILEToken]): Either[WHILEParserError, WHILEProgram] = {
    println(tokens)
    val reader = new WHILETokenReader(tokens)
    phrase(composition | plus | minus | loop | while_)(reader) match {
      case NoSuccess(msg, next)  ⇒ Left(WHILEParserError(msg + " at line " + next.pos.line + ", column " + next.pos.column + "\r\n" + next.pos.longString))
      case Success(result, next) ⇒ Right(result)
    }
  }

}

object WHILECompiler {

  def apply(code: String) = {
    for {
      tokens ← WHILELexer(code).right
      program ← WHILEParser(tokens).right
    } yield program
  }

}
