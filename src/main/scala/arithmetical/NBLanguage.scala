package arithmetical

//import scala.util.parsing.input.StreamReader

object NBLanguage extends NBLanguageParser with Reducers with RetryWithFunction  {

  def main(args: Array[String]): Unit = {

   val line = scala.io.StdIn.readLine()

    println("Line: " + line)

    val tokens = new lexical.Scanner(line)
    phrase(Expr)(tokens) match {
      case Success(tree, _) =>
        val newTree = retry(tree)(reduceGrammar)

        if (!validResult(newTree)) println("Stuck term: " + newTree)

        print("Big step: ")
        val bigStepResultTree = bigStepReduce(tree)
        if (!validResult(bigStepResultTree)) print("Stuck term: ")
        println(bigStepResultTree)

      case err => println(err)
    }
  }

}