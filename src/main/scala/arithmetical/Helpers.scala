package arithmetical

trait Helpers {

  /**
    * Check if succ succ succ..0 or just 0
    */
  def isNumericValue(t: Term): Boolean = t match {
    case _:ZeroValue => true
    case t:Succ => t.isNumeric
    case _ => false
  }

  /**
    * Check if the result valid
    * @param t: Term
    * @return
    */
  def validResult(t: Term): Boolean = t match {
    case _: BoolValue => true
    case other => isNumericValue(other)
  }

  /**
    * Checks if the stuck term is valid result, in which case it could not be reduced because of the surrounding term
    * and in that case the node is returned
    * @param term "inside" term
    * @param node term "surrounding" the term
    * @return the smallest part of the AST that could not be reduced
    */
  def checkStuckTerm(term: Term, node: Term): Term = if (validResult(term)) node else term

}

