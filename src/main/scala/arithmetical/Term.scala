package arithmetical

import scala.util.parsing.input.Positional
import NBLanguage.isNumericValue

/** Abstract Syntax Trees for terms. */
sealed trait Term extends Positional

case class IsZero(t: Term) extends Term
case class Succ(t:Term) extends Term {
  def isNumeric: Boolean = isNumericValue(t)
}
case class Pred(t: Term) extends Term

/**
  * Value is true, false and NV
  */
trait Value extends Term
case class BoolValue(x: Boolean) extends Value {
  override def toString: String = if (x) "True" else "False"
}
case class ZeroValue() extends Value {
  override def toString: String = "Zero"
}
/**
  * The If grammar structure
  * @param e1 the condition term
  * @param e2 then branch term
  * @param e3 else branch term
  */
case class IfThenElse(e1: Term, e2: Term, e3: Term) extends Term {
  override def toString: String = "If(" + e1 +  ", " + e2 +  ", " + e3 + ")"
}