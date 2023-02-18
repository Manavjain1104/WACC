package wacc

import parsley.io.ParseFromIO
import parsley.{Failure, Success}
import wacc.SemTypes.SemType
import wacc.lexer.fully
import wacc.parser.program
import wacc.errorPrinter._

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

object Main {

  final val SYNTAX_ERROR_CODE = 100
  final val SEMANTIC_ERROR_CODE = 200
  final val OK_EXIT_CODE = 0

  final val EXT_LENGTH = 5

  def main(args: Array[String]): Unit = {

    println("-- Compiling...")

    val sem: semanticAnalyser = new semanticAnalyser
    val file: File = new File(args.head)

    implicit val eb: error.SyntaxErrorBuilder = new error.SyntaxErrorBuilder
    fully(program).parseFromFile(file) match {
      case util.Success(value) => {
        value match {
          case Success(prog) => {
            val topST = new SymbolTable[SemType](None)
            val errLog: Option[ListBuffer[error.SemanticError]] = sem.checkProgram(prog, topST)

            if (errLog.isDefined) {
              if (args.length > 1) {
                val test = args(1)
                if (test == "check") {
                  println("returning 200")
                  return
                }
              }
              generateOutputMessages(errLog.get, None, file.getPath, SEMANTIC_ERROR_CODE)
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
              generateOutputMessages(ListBuffer.empty[error.SemanticError], None, file.getPath, OK_EXIT_CODE)
//              println(value)

              // TODO : Code Generation
              val output = armPrinter.print(new codeGenerator(prog))
//              new codeGenerator(prog).generateProgIR().foreach(ir => println(ir))
//              println(output)

              // Write assembly to .s file
              val writerToFile = new PrintWriter(new File(file.getName.dropRight(EXT_LENGTH - 1) + ".s"))
              writerToFile.write(output)
              writerToFile.close()

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
  }

}