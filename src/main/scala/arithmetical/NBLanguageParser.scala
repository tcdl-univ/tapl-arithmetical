package arithmetical

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
  * This object implements a parser and a reducer for the NB
  * language of booleans and numbers found in Chapter 3 of
  * the TAPL book.
  */
class NBLanguageParser extends StandardTokenParsers {
  // The StandardTokenParsers reserved tokens
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")


  /**
    * We need to define the expression term which will be threated by the StandardTokenParsers as
    * Expr ::= 'true'
    * | 'false'
    * | 'if' Expr 'then' Expr 'else' Expr
    * | '0'
    * | 'succ' Expr
    * | 'pred' Expr
    * | 'iszero' Expr
    */
  def Expr: Parser[Term] = (
    "true" ^^^ BoolValue(true)
      | "false" ^^^ BoolValue(false)
      | "if" ~ Expr ~ "then" ~ Expr ~ "else" ~ Expr ^^ { case "if" ~ e1 ~ "then" ~ e2 ~ "else" ~ e3 => IfThenElse(e1, e2, e3) }
      | (nv | "succ" ~> Expr ^^ { e1 => Succ(e1) })
      | "pred" ~> Expr ^^ { e1 => Pred(e1) }
      | "iszero" ~> Expr ^^ { e1 => IsZero(e1) }
      | failure("illegal start of expression")
    )

  /**
    * NV ::= numericLit | succ NV
    * We allow numeric values greater than 0 as well in this parser
    * @return parsed term
    */
  def nv: Parser[Term] =
    rep("succ") ~ numericLit ^^ {
      case list ~ numLit => syntacticSugar(list.size + numLit.toInt)
    }

  /**
    * Decompose the numeric values to succs succ(succ(succ...(0)..))
    * @param x value read by parser, can be any value greater or equal to 0
    * @return either [[ZeroValue]] or [[Succ]] term
    */
  def syntacticSugar(x: Int): Term = if (x == 0) ZeroValue() else Succ(syntacticSugar(x - 1))
}
