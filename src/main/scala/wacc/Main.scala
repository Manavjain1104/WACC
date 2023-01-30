package wacc

import parsley.{Failure, Success}

import scala.language.postfixOps


object Main {

  import parser.expr

  def main(args: Array[String]): Unit = {
    println("Hello WACC_25!")

    expr.parse(args.head) match {
      case Success(x) => println(s"${args.head} = $x")
      case Failure(msg) => println(msg)
    }
  }
}