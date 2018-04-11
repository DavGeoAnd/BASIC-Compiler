package csp.ch03

// From SBT: ~run-main csp.ch03.Project

object Project {
  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val keywords : List[String] = List ("let", "in", "end")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
	val string : Parser[Unit] = P(""" ~ (alpha | integer).rep ~ """).map(s => ())
	
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace (" ".rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val putYourStuffHere : Parser[Unit] = P ("do some stuff here")
	
	val declareStatement : Parser[Unit] = P ("dim" ~ ident ~ "as" ~ alpha.rep ~ "=" ~ (integer | string)).map (s => ())
	val addSubStatement : Parser[Unit] = P ((ident | integer) ~ ("+" | "-") ~ (ident | integer)).map (s => ())
	val concatStatement : P[Unit] = P (ident ~ "&=" ~ string).map (s => ())
	
	val regStatements : Parser[Unit] = P (declareStatement | addSubStatement | concatStatement).map (s => ())
	
	val whileLoop : P[Unit] = P ("while" ~ ident ~ regStatements ~ "wend").map (s => ())
	val forLoop : P[Unit] = P ("for" ~ ident ~ "as" ~ "integer" ~ "=" ~
								integer ~ "to" ~ integer ~ 
								regStatements ~ "next" ~ ident).map (s => ())
	val ifElseStatement : P[Unit] = P("If" ~ ident ~ "Then" ~ regStatements ~ "Else" ~ 
								  regStatements ~ "End If").map (s => ())
	
	val loops : P[Unit] = P(whileLoop | forLoop)
	
	//val start : Parser[Unit] = P (putYourStuffHere ~ End)
	
  }

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Let (nm : String, e1 : Expr, e2 : Expr)  extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  import fastparse.all.{Parsed,Parser}

  def test (p : Parser[Unit], s : String) : Unit = {
    val result : fastparse.core.Parsed[Unit, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    //val p01 : Parser[Unit] = MyParsers.start
    //test (p01, "do some stuff here")
	
	test (MyParsers.regStatements, "dim a as integer = 3")
	test (MyParsers.regStatements, "dim a as integer = 235")
	test (MyParsers.regStatements, "dim s as string = "Str"")
	test (MyParsers.regStatements, "dim s as string = "Hello123"")
	test (MyParsers.regStatements, "a + 1")
	test (MyParsers.regStatements, "a - 15")
	test (MyParsers.regStatements, "a &= "Yo"")
	test (MyParsers.loops, "while i i - 1 wend")
	test (MyParsers.loops, "for c as integer = 0 to 10 c + 1 next c")
	test (MyParsers.ifElseStatement, "If x Then x + 1 Else x - 1 End If")
	
    println ("=" * 80)
  }
}