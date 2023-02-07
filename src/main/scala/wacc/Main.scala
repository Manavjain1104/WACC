package wacc

import scala.io.Source
import parsley.{Failure, Success}
import wacc.lexer.fully
import wacc.parser.{expr, func, lvalue, program, rvalue, statement, waccType}
import scala.Boolean

object Main {

  def main(args: Array[String]): Unit = {

    //    println("hi")
    //    println(args.head)

    val sem: semantic_analyser = new semantic_analyser
    val fileContents = Source.fromFile(args.head)
    val text: String = fileContents.getLines().mkString("\n")
    //println(text)
    fully(program).parse(text) match {
      case Success(x) => {
        //println(s"$x")
        //println("Passing code to semantics :-")
        //println(x)

        val topST = new SymbolTable(None)
        val retST = sem.checkProgram(x, topST)

        if (retST.isDefined) {
          if (args.length > 1) {
            val test = args(1)
            if (test == "check") {
              println("returning 0")
              return
            }
          }
          println("ALL GOOD")
          sys.exit(0)
        } else {
          if (args.length > 1) {
            val test = args(1)
            if (test == "check") {
              println("returning 200")
              return
            }
          }
          println("SEMANTIC ERROR")
          sys.exit(200)
        }
      }


      case Failure(msg) => {
        if (args.length > 1) {
          val test = args(1)
          if (test == "check") {
            println("returning 100")
            return
          }
        }
        println("Syntax error!")
        //println(msg)
        println("Exit code: 100")
        sys.exit(100)
      }
    }


//        val fileContents = Source.fromFile("/Users/krishmaha/wacc/new/WACC_25/src/main/scala/wacc/testProg.txt")
//        val text : String = fileContents.getLines.mkString("\n")
//        println(text)
//        fully(program).parse(text) match {
//          case Success(x) => println(s"$x")
//          case Failure(msg) => println(msg)
//        }
//    ////    fileContents.close()
//    program.parse(args.head) match {
//      case Success(x) => println(s"${args.head} = $x")
//      case Failure(msg) => println(msg)
//    }
    //        lvalue.parse(args.head) match {
    //          case Success(x) => println(s"${args.head} = $x")
    //          case Failure(msg) => println(msg)
    //        }
    //    //   rvalue.parse(args.head) match {
    //    //          case Success(x) => println(s"${args.head} = $x")
    //    //          case Failure(msg) => println(msg)
    //    //        }
//    fully(expr).parse(args.head) match {
//      case Success(x) => println(s"${args.head} = $x")
//      case Failure(msg) => println(msg)
//    }
//        statement.parse(args.head) match {
//          case Success(x) => println(s"${args.head} = $x")
//          case Failure(msg) => println(msg)
//        }
  }

}