package wacc

import scala.io.Source
import parsley.{Failure, Success}
import wacc.lexer.fully
import wacc.parser.{expr, func, lvalue, program, rvalue, waccType}

object Main {

  def main(args: Array[String]): Unit = {
    val fileContents = Source.fromFile("/Users/ruchit/Imperial/wacc/WACC_25/src/main/scala/wacc/testProg.txt")
    val text : String = fileContents.getLines.mkString("\n")
    println(text)
    fully(program).parse(text) match {
      case Success(x) => println(s"$x")
      case Failure(msg) => println(msg)
    }
    fileContents.close()

//            program.parse(args.head) match {
//              case Success(x) => println(s"${args.head} = $x")
//              case Failure(msg) => println(msg)
//            }
    ////    lvalue.parse(args.head) match {
    ////      case Success(x) => println(s"${args.head} = $x")
    ////      case Failure(msg) => println(msg)
    ////    }
    //    //   rvalue.parse(args.head) match {
    //    //          case Success(x) => println(s"${args.head} = $x")
    //    //          case Failure(msg) => println(msg)
    //    //        }
    ////        fully(expr).parse(args.head) match {
    ////          case Success(x) => println(s"${args.head} = $x")
//          case Failure(msg) => println(msg)
//       }
  }
}