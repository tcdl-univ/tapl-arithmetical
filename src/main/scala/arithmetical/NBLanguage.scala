package arithmetical

//import scala.util.parsing.input.StreamReader

object NBLanguage extends NBLanguageParser with Reducers  {

  def main(args: Array[String]): Unit = {
    val inputs = Array("iszero 0",
      "if iszero 0 then true else false",
      "iszero pred succ 0",

      "if pred succ succ 0 then if iszero 0 then true else false else succ succ succ succ 0",
      "if if true then iszero pred succ succ 0 else pred 0 then iszero pred succ pred succ 0 else if pred succ succ pred succ 0 then true else false")

    inputs.foreach { line =>

      println("Line: " + line)

      val tokens = new lexical.Scanner(line)
      phrase(Expr)(tokens) match {
        case Success(tree, _) =>
          var oldTree: Term = null
          var newTree = tree

          print("\n")
          while (oldTree != newTree) {
            println(newTree)
            oldTree = newTree
            newTree = reduceGrammar(oldTree)
          }

          if (!validResult(newTree)) println("Stuck term: " + newTree)

          print("Big step: ")
          val bigStepResultTree = bigStepReduce(tree)
          if (!validResult(bigStepResultTree)) print("Stuck term: ")
          println(bigStepResultTree)

        case err => println(err)
      }
    }
  }
}