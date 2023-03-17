package wacc

import parsley.io.ParseFromIO
import parsley.{Failure, Success}
import wacc.SemTypes.SemType
import wacc.lexer.fully
import wacc.parser.program
import wacc.errorPrinter._

import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import scala.collection.mutable.ListBuffer

object Main {

  final val SYNTAX_ERROR_CODE = 100
  final val SEMANTIC_ERROR_CODE = 200
  final val OK_EXIT_CODE = 0

  final val EXT_LENGTH = 6

  def main(args: Array[String]): Unit = {

    //    println("-- Compiling...")

    val sem: semanticAnalyser = new semanticAnalyser

    var file: File = new File("")
    try {
      file = new File(args.head)
    } catch {
      case e: FileNotFoundException => {
        System.err.println("Couldn't find that file.")
        sys.exit(-1)
      }
      case e: IOException => {
        System.err.println("Had an IOException trying to read that file")
        sys.exit(-1)
      }
      case _: Exception => {
        System.err.println("Error opening file")
        sys.exit(-1)
      }
    }

    implicit val eb: error.SyntaxErrorBuilder = new error.SyntaxErrorBuilder

    var optimiseFlag = true
    var inliningFlag = true

    // optimiseFlag is FIRST argument AFTER file.
    // inliningFlag is SECOND argument AFTER file.


    if (args.length >= 2 && args(1).contains("-true")) {
      try {
        optimiseFlag = args(1).tail.toBoolean
      } catch {
        case _: IllegalArgumentException =>
      }
    }

    if (args.length == 3 && args(2).contains("-true")) {
      try {
        inliningFlag = args(2).tail.toBoolean
      } catch {
        case _: IllegalArgumentException =>
      }
    }
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
                  return
                }
              }
              generateOutputMessages(ListBuffer.empty[error.SemanticError], None, file.getPath, OK_EXIT_CODE)

              val output = armPrinter.print(new codeGenerator(prog, optimiseFlag, inliningFlag))

              // Write assembly to .s file
              if (!file.getName.endsWith(".wacc")) {
                println("Check that you are targeting a WACC program.")
              }
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