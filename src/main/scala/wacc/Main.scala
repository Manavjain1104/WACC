package wacc

import parsley.io.ParseFromIO
import parsley.{Failure, Success}
import wacc.lexer.fully
import wacc.parser.program
import wacc.printer._

import java.io.File
import scala.collection.mutable.ListBuffer

object Main {

  final val SYNTAX_ERROR_CODE = 100
  final val SEMANTIC_ERROR_CODE = 200
  final val OK_EXIT_CODE = 0

  def main(args: Array[String]): Unit = {

    println("-- Compiling...")

    val sem: semantic_analyser = new semantic_analyser
    val file: File = new File(args.head)

    implicit val eb: error.SyntaxErrorBuilder = new error.SyntaxErrorBuilder
    fully(program).parseFromFile(file) match {
      case util.Success(value) => {
        value match {
          case Success(prog) => {
            val topST = new SymbolTable(None)
            val errLog: Option[ListBuffer[error.SemanticError]] = sem.checkProgram(prog, topST)

            if (errLog.isDefined) {
              if (args.length > 1) {
                val test = args(1)
                if (test == "check") {
                  println("returning 200")
                  return
                }
              }
              println(generateOutputMessages(errLog.get, None, file.getPath, SEMANTIC_ERROR_CODE))
              sys.exit(SEMANTIC_ERROR_CODE)
            } else {
              // semantic check passed
              if (args.length > 1) {
                val test = args(1)
                if (test == "check") {
                  println("returning 0")
                  return
                }
              }
              println("Compilation Successful!")
              sys.exit(OK_EXIT_CODE)
            }
          }

          case Failure(syntaxErr) => {
            if (args.length > 1) {
              val test = args(1)
              if (test == "check") {
                println("returning 100")
                return
              }
            }
            println("Syntax error!")
            generateOutputMessages(ListBuffer.empty[error.SemanticError], Some(syntaxErr), file.getPath, SYNTAX_ERROR_CODE)
            sys.exit(SYNTAX_ERROR_CODE)
          }
        }
      }
      case util.Failure(exception) => {
        System.err.println("Cannot read from input file ", file.getPath)
        System.err.println(exception)
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