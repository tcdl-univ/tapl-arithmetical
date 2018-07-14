import org.scalatest._
import org.scalatest.matchers.{MatchResult, Matcher}
import arithmetical.NBLanguage.{bigStepReduce, reduceGrammar, validResult}
import arithmetical.{BoolValue, IfThenElse, IsZero, Pred, Succ, Term, ZeroValue}
import arithmetical.RetryWithFunction


class ReducerTestSpec extends FreeSpec with Matchers {

  "reduceGrammar" - {
    "one step reducer" - {
      "simple reduction" in {
        val initialTree = IsZero(Pred(Succ(ZeroValue())))

        initialTree shouldNot beReducedOneStepTo(initialTree)
        initialTree should beReducedOneStepTo(IsZero(ZeroValue()))
      }

      "Non reductible expression" in {
        val initialTree = ZeroValue()

        initialTree should beReducedOneStepTo(initialTree)
      }
    }

    "multiple step" - {

      "valid reducible expressions to a valid result" - {
        "Reduce the expression to a valid one" in {
          val initialTree = IsZero(Pred(Succ(ZeroValue())))

          initialTree shouldNot beReducedWithNoStuck(initialTree)
          initialTree shouldNot beReducedWithNoStuck(IsZero(ZeroValue()))
          initialTree should beReducedWithNoStuck(BoolValue(true))
        }
      }

      "Struck expressions with reduceGrammar" - {
        "unreducible to an immediate eval" in {
          val initialTree = IfThenElse(Pred(Succ(Succ(ZeroValue()))), IfThenElse(IsZero(ZeroValue()), BoolValue(true),
            BoolValue(false)), Succ(Succ(Succ(Succ(ZeroValue())))))

          val expected = IfThenElse(Succ(ZeroValue()), IfThenElse(IsZero(ZeroValue()), BoolValue(true),
            BoolValue(false)), Succ(Succ(Succ(Succ(ZeroValue())))))

          initialTree shouldNot beReducedWithNoStuck(BoolValue(true))
          initialTree should beReducedWithNoStuck(expected)
        }

        "unreducible 2" in {
          val initialTree = IfThenElse(IfThenElse(BoolValue(true), IsZero(Pred(Succ(Succ(ZeroValue())))),
            Pred(ZeroValue())), IsZero(Pred(Succ(Pred(Succ(ZeroValue()))))), IfThenElse(Pred(Succ(Succ(Pred(Succ(ZeroValue()))))),
            BoolValue(true), BoolValue(false)))

          val expected = IfThenElse(IsZero(Succ(ZeroValue())), IsZero(Pred(Succ(Pred(Succ(ZeroValue()))))),
            IfThenElse(Pred(Succ(Succ(Pred(Succ(ZeroValue()))))), BoolValue(true), BoolValue(false)))

          initialTree should beReducedWithNoStuck(expected)
        }
      }

      " with bigStepReduce" - {
        "unreducible to an immediate eval" in {
          val initialTree = IfThenElse(Pred(Succ(Succ(ZeroValue()))), IfThenElse(IsZero(ZeroValue()), BoolValue(true),
            BoolValue(false)), Succ(Succ(Succ(Succ(ZeroValue())))))

          val expected = IfThenElse(Succ(ZeroValue()), IfThenElse(IsZero(ZeroValue()), BoolValue(true),
            BoolValue(false)), Succ(Succ(Succ(Succ(ZeroValue())))))

          initialTree should beReducedWithBigStep(expected)
        }

        "unreducible 2" in {
          val initialTree = IfThenElse(IfThenElse(BoolValue(true), IsZero(Pred(Succ(Succ(ZeroValue())))),
            Pred(ZeroValue())), IsZero(Pred(Succ(Pred(Succ(ZeroValue()))))), IfThenElse(Pred(Succ(Succ(Pred(Succ(ZeroValue()))))),
            BoolValue(true), BoolValue(false)))

          val expected = Pred(Succ(Succ(Pred(Succ(ZeroValue())))))

          initialTree should beReducedWithBigStep(expected)
        }
      }
    }
  }

}

case class beReducedOneStepTo[T](expect: Term) extends Matcher[Term] {
  def apply(left: Term) = {
    val reducedTerm = reduceGrammar(left)

    MatchResult(reducedTerm == expect,
      s"Parsing $reducedTerm did not equal $expect",
      s"Parsing $reducedTerm was equal to $expect")
  }

}

case class beReducedWithBigStep[T](expect: Term) extends Matcher[Term] with RetryWithFunction {
  def apply(left: Term) = {
    val reducedTerm = retry(left)(reduceGrammar)
    val reduced = bigStepReduce(reducedTerm)

    MatchResult(reduced == expect,
      s"Parsing $reduced did not equal $expect",
      s"Parsing $reduced was equal to $expect")
  }

}

case class beReducedWithNoStuck[T](expect: Term) extends Matcher[Term] with RetryWithFunction {
  def apply(left: Term) = {
    val reducedTerm = retry(left)(reduceGrammar)

    //TODO: try to pass the reducer, parser to a Try as we need to model this as a monadic struct.
    MatchResult(reducedTerm == expect,
      s"Parsing $reducedTerm did not equal $expect",
      if (validResult(reducedTerm))
        s"Parsing $reducedTerm was equal to $expect."
      else
        s"Parsing is an struck expr.")
  }

}


