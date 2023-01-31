package wacc

import parsley.{Failure, Success}
import wacc.parser.{lvalue, rvalue, waccType, expr}

object Main {

  def main(args: Array[String]): Unit = {
    println("Hello WACC_25!")


//    waccType.parse(args.head) match {
//      case Success(x) => println(s"${args.head} = $x")
//      case Failure(msg) => println(msg)
//    }
//    lvalue.parse(args.head) match {
//      case Success(x) => println(s"${args.head} = $x")
//      case Failure(msg) => println(msg)
//    }
//   rvalue.parse(args.head) match {
//          case Success(x) => println(s"${args.head} = $x")
//          case Failure(msg) => println(msg)
//        }
    expr.parse(args.head) match {
      case Success(x) => println(s"${args.head} = $x")
      case Failure(msg) => println(msg)
    }
  }
}