import org.scalatest._
import org.scalatest.matchers.{MatchResult, Matcher}
import arithmetical.NBLanguage._
import arithmetical.{BoolValue, IfThenElse, IsZero, Pred, Succ, Term, ZeroValue}

class ParserTestSpec extends FreeSpec with Matchers {

  "Parser" - {
    "valid parsing results" - {
      "Should parse a very simple expression" in {
        val expected = ParserSuccess(IsZero(ZeroValue()))
        "iszero 0" should beParsedTo(expected)
      }

      "An if/else expression should be parsed to a valid term" in {
        val expected = ParserSuccess(IfThenElse(IsZero(ZeroValue()), BoolValue(true), BoolValue(false)))
        "if iszero 0 then true else false" should beParsedTo(expected)
      }

      "be parsing a reducible expression" in {
        val expected = ParserSuccess(IsZero(Pred(Succ(ZeroValue()))))
        "iszero pred succ 0" should beParsedTo(expected)
      }

      "be parsing an struck expr but valid syntactically" in {
        val expected = ParserSuccess(IfThenElse(Pred(Succ(Succ(ZeroValue()))), IfThenElse(IsZero(ZeroValue()),
          BoolValue(true), BoolValue(false)), Succ(Succ(Succ(Succ(ZeroValue()))))))
        "if pred succ succ 0 then if iszero 0 then true else false else succ succ succ succ 0" should beParsedTo(expected)
      }

      "parsing an struck expr valid 2" in {
        val expected = ParserSuccess(IfThenElse(IfThenElse(BoolValue(true), IsZero(Pred(Succ(Succ(ZeroValue())))),
          Pred(ZeroValue())), IsZero(Pred(Succ(Pred(Succ(ZeroValue()))))), IfThenElse(Pred(Succ(Succ(Pred(Succ(ZeroValue()))))),
          BoolValue(true), BoolValue(false))))
        "if if true then iszero pred succ succ 0 else pred 0 then iszero pred succ pred succ 0 else if pred succ succ pred succ 0 then true else false" should
          beParsedTo(expected)
      }
    }

    "failing parsing" - {
      "not parse an invalid expression" in {
        "iszero succ succ bleh" should beParsedTo(ParserError("illegal start of expression"))
      }
    }
  }

}


case class beParsedTo[T](expect: ParserResult) extends Matcher[String] {
  def apply(left: String) = {
    val tokens = new lexical.Scanner(left)

    val result = phrase(Expr)(tokens) match {
      case Success(tree, _) => ParserSuccess(tree)
      case Error(msg, _) => ParserError(msg)
      case Failure(msg, _) => ParserError(msg)
    }

    MatchResult(result == expect,
      s"Parsing $result did not equal $expect",
      s"Parsing $result was equal to $expect")
  }
}

sealed trait ParserResult

case class ParserSuccess(tree: Term) extends ParserResult

case class ParserError(msg: String) extends ParserResult