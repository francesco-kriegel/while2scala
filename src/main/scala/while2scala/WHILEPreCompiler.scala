package while2scala

import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed trait WHILEPreToken
case class MACRO(notation: String, code: String, regex: Regex, parser: WHILEPreCompiler.Parser[String])
case class MAIN(code: String)

object WHILEPreCompiler extends RegexParsers {

  override def skipWhitespace = false
  override val whiteSpace = "[ \t\r\f\n]+".r

  val block = "(?s)((?!DEF)(?!BY)(?!YB).)*".r

  def main_ : Parser[MAIN] = {
    block ^^ { s ⇒ MAIN(s) }
  }

  val xvarreg = "x[0-9]+".r
  val xvarpar = "x[a-zA-Z0-9]+".r ^^ { s ⇒ (s, xvarreg) }
  //  val yvarpar = "y[a-zA-Z0-9]+".r ^^ { s ⇒ (s, "y[0-9]+".r) }
  val xcodepar = "((?!x[a-zA-Z0-9]+)[^ ])+".r ^^ { s ⇒ (s, Regex.quote(s).r) }
  //  val xycodepar = "(?s)((?!x[a-zA-Z0-9]+)(?!y[a-zA-Z0-9]+).)+".r ^^ { s ⇒ (s.replaceAll("[ \t\r\f\n]+", " "), (s.replaceAll("[ \t\r\f\n]+", " ").split(" ").foldLeft("[ \t\r\f\n]*")((x, y) ⇒ x + Regex.quote(y) + "[ \t\r\f\n]+").reverse.substring(1).reverse + "*").r) }
  val subblock = (Regex.quote("{") + "(?s).+" + Regex.quote("}")).r
  val ptemplate = Regex.quote("{P}").r ^^ { s ⇒ (s, subblock) }
  val whitespace = whiteSpace ^^ { s ⇒ (s, whiteSpace) }

  def macro_ : Parser[MACRO] = {
    opt(whitespace) ~> "DEF" ~ block ~ "BY" ~ block ~ "YB" <~ opt(whitespace) ^^ {
      case _ ~ f ~ _ ~ x ~ _ ⇒ {
        val fre = parse(opt(whitespace) ~> rep(whitespace | xvarpar | ptemplate | xcodepar) <~ opt(whitespace), f.trim).get
        val fregex = fre.map(_._2).mkString.r
        //        val fregex = fre.tail.map(_._2.regex).foldLeft(fre.head._2.regex)(_ + _).r
        var fpar: Parser[(Map[String, String], String)] = regex(fre.head._2) ^^ { t ⇒
          {
            if (fre.head._2 equals xvarreg)
              (Map(fre.head._1 -> t), t)
            else if (fre.head._2 equals subblock)
              (Map(Regex.quote("{P}") -> t.substring(1).reverse.substring(1).reverse), t)
            else
              (Map(), t)
          }
        }
        for (r ← fre.tail)
          fpar = ((fpar ~ regex(r._2)) filter {
            case (s1, s2) ~ t ⇒ !(r._2.equals(xvarreg) && s1.contains(r._1) && !s1(r._1).equals(t))
          }) ^^ {
            case (s1, s2) ~ t ⇒ {
              if (r._2 equals xvarreg)
                (s1 + (r._1 -> t), s2 + t)
              else if (r._2 equals subblock)
                (s1 + (Regex.quote("{P}") -> t.substring(1).reverse.substring(1).reverse), s2 + t)
              else
                (s1, s2 + t)
            }
          }
        val finalfpar: Parser[String] = opt(whitespace) ~> fpar <~ opt(whitespace) ^^ {
          case (map, str) ⇒
            {
              var y = x
              for ((key, value) ← map) {
                println("replacing " + key + " with " + value)
                y = y.replaceAll(key, value)
              }
              "\r\n" + y.trim + "\r\n"
            }
        }

        //        println(x)
        //        val xre = parse(rep(whitespace | xvarpar | yvarpar | xycodepar), x).get
        //        var xpar: Parser[Any] = regex(xre.head._2) ^^ { t ⇒ println(xre.head._1 + " = " + t) }
        //        for (r ← xre.tail)
        //          xpar = xpar ~ (regex(r._2) ^^ { t ⇒ println(r._1 + " = " + t) })
        //        println(parse(xpar, "x5:=y1+0;\r\n    LOOP x7 DO\r\n x5:=x5+x6\r\n END").successful)
        //        println()

        MACRO(f.trim, x.trim, fregex, finalfpar)
      }
    }
  }

  def apply(code: String): Either[WHILEPreCompilerError, String] = {
    parse(rep(macro_) ~ main_, code) match {
      case NoSuccess(msg, next) ⇒ Left(WHILEPreCompilerError(msg + " at line " + next.pos.line + ", column " + next.pos.column + "\r\n" + next.pos.longString))
      case Success(result, next) ⇒ {
        result match {
          case macros ~ main ⇒ {
            //            println(macros)
            var nextxvar = xvarreg.findAllIn(code).toSet[String].map(xvar ⇒ xvar.substring(1).toInt).fold(1)(scala.math.max)
            if (macros.isEmpty)
              return Right(code)
            var par: Parser[String] = null
            if (macros.tail.isEmpty) par = macros.head.parser
            else par = macros.tail.map(_.parser).foldLeft(macros.head.parser)((p, q) ⇒ p | q)
            val macroparser: Parser[String] = phrase(((rep(
              (
                (opt(whitespace) ~> par <~ opt(whitespace) ^^ {
                  macrocode ⇒
                    {
                      var expandedcode = macrocode
                      val xmap = "x[a-zA-Z]+".r.findAllIn(macrocode).toSet[String].map(xvar ⇒ {
                        nextxvar += 1
                        (xvar, "x" + nextxvar)
                      }).foldLeft(Map[String, String]())(_ + _)
                      for ((key, value) ← xmap)
                        expandedcode = expandedcode.replaceAll(key, value)
                      expandedcode
                    }
                }) | "(?s).+?".r)) ^^ { l ⇒
                {
                  if (l.isEmpty) ""
                  else if (l.tail.isEmpty) l.head
                  else l.tail.foldLeft(l.head)((x, y) ⇒ x + "" + y)
                }
              }) ~ "(?s).*".r) ^^ { case x ~ y ⇒ x + y })

            var result0: String = null
            var result1: String = main.code
            while (!(result1 equals result0)) {
              var code = result1
              //              println("intermediate code:")
              //              println(code)
              //              println()
              result0 = result1
              parse(macroparser, code) match {
                case NoSuccess(msg, next) ⇒ return Left(WHILEPreCompilerError(msg + " at line " + next.pos.line + ", column " + next.pos.column + "\r\n" + next.pos.longString))
                case Success(result, next) ⇒ {
                  result1 = result
                }
              }
            }
            Right(result0)
          }
        }
      }
    }
  }

}
