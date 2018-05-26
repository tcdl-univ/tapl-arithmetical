package arithmetical

trait Reducers extends Helpers{

  /**
    * Computation and congruence rules applied
    * @param node AST that is meant to be reduced
    * @return reduced AST or stuck term
    */
  def reduceGrammar(node: Term): Term = {
    node match {
      case IfThenElse(e1, e2, e3) =>
        e1 match {
          case e1:BoolValue => if(e1.x) e2 else e3
          case _ => IfThenElse(reduceGrammar(e1), e2, e3)
        }

      case IsZero(e1) =>
        e1 match {
          case ZeroValue() => BoolValue(true)
          case Succ(e2) => if (e1.asInstanceOf[Succ].isNumeric) BoolValue(false) else IsZero(Succ(reduceGrammar(e2)))
          case _ => IsZero(reduceGrammar(e1))
        }

      case Pred(e1) =>
        e1 match {
          case ZeroValue() => e1
          case Succ(e2) =>
            e1 match {
              case e1: Succ if e1.isNumeric => e2
              case _ => Pred(Succ(reduceGrammar(e2)))
            }
          case _ => Pred(reduceGrammar(e1))
        }

      case Succ(e1) =>
        e1 match {
          case ZeroValue() => node
          case Succ(e2) =>
            e1 match {
              case e1:Succ if e1.isNumeric => node
              case _ => Succ(Succ(reduceGrammar(e2)))
            }
          case _ => Succ(reduceGrammar(e1))
        }

      case _ => node
    }
  }

  /**
    * Big step semantics applied to the term
    * @param node AST that we want to reduce
    * @return reduced AST or stuck term
    */
  def bigStepReduce(node: Term): Term = {
    node match {
      case BoolValue(_) => node
      case ZeroValue() => node
      case Succ(e1) =>
        val e1Reduced = bigStepReduce(e1)
        if (isNumericValue(e1Reduced)) Succ(e1Reduced) else checkStuckTerm(e1Reduced, node)
      case IfThenElse(e1, e2, e3) =>
        val e1Reduced = bigStepReduce(e1)

        e1Reduced match {
          case e1Red: BoolValue if e1Red.x => bigStepReduce(e2)
          case _: BoolValue => bigStepReduce(e3)
          case _ => checkStuckTerm(e1Reduced, node)
        }

      case Pred(e1) =>
        val e1Reduced = bigStepReduce(e1)
        reducePred(e1Reduced, e1, node)

      case IsZero(e1) =>
        val e1Reduced = bigStepReduce(e1)
        reduceIsZero(e1Reduced, node)

      case _ => node
    }
  }

  def reducePred(e1Reduced: Term, e1: Term, node: Term):Term = e1Reduced match {
    case e1Red:ZeroValue => e1Red
    case e1Red if isNumericValue(e1) => e1Red.asInstanceOf[Succ].t
    case e1Red => checkStuckTerm(e1Red, node)
  }

  def reduceIsZero(e1Reduced: Term, node: Term):Term = e1Reduced match {
    case _: ZeroValue => BoolValue(true)
    case e1Red if isNumericValue(e1Red) => BoolValue(false)
    case e1Red => checkStuckTerm(e1Red, node)
  }

}
