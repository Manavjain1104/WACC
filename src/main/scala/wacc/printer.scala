package wacc

import wacc.SemTypes.SemType
import wacc.error._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource
import scala.io.Source._
import scala.math

object printer {

  final val SYNTAX_ERROR_CODE = 100
  final val SEMANTIC_ERROR_CODE = 200
  final val OK_EXIT_CODE = 0

  def generateOutputMessages(errorLog : ListBuffer[SemanticError], filename : String, exitCode : Int): String = {
    if (exitCode == OK_EXIT_CODE) {
      return "---- Compilation success. Exit code 0 returned ---- \n"
    }

    val sb = new StringBuilder()

    if (exitCode == SYNTAX_ERROR_CODE) {
      sb.append("---- Syntax Error found. Exit code 100 returned ---- \n")
      // TODO : write syntax printer
      return sb.toString()
    }

    // semantic error(s) case
    assert(exitCode == SEMANTIC_ERROR_CODE)
    val smp = SemanticPrinter(errorLog, filename)
    sb.append("---- Semantic Error(s) found. Exit code 200 returned ---- \n")
    sb.append(smp.print())
    sb.toString()
  }

  def getLinesAround(lineNum : Int, noOfLines: Int, filename : String, lineFromFile : List[String]): List[String] = {
    val out = collection.mutable.ListBuffer.empty[String]
    for (i <- (math.max(0, lineNum - noOfLines) to math.min(lineFromFile.length - 1, lineNum + noOfLines))) {
      out += lineFromFile(i)
    }
    out.toList
  }


  sealed trait Printer {
    def print(): String
  }

  case class SemanticPrinter(errorLog : ListBuffer[SemanticError], filename : String) extends Printer {

    // initial constructor code
    val source: BufferedSource = fromFile(filename)
    val fileLines: List[String] = source.getLines().toList
    source.close()

    def getIdentPos(pos: (Int, Int), ident: String): (Int, Int) = {
      var line = pos._1 - 1
      var col = pos._2 - 1
      val len = ident.length
      while (!fileLines(line).slice(col, col + len).equals(ident)) {
        col += 1
        if (col >= fileLines(line).length || fileLines(line).charAt(col) == '\n') {
          col = 0
          line += 1
        }
      }
      (line + 1, col + 1)
    }

    def print(): String = {
      val sb = new StringBuilder
      for (error <- errorLog) {
        error match {
          case UnknownIdentifierError(pos, ident, context) => {
            sb.append("Unknown Identifier error in " + filename + " " + "(line " + pos._1 + ", coloumn " + pos._2 + "):\n")
            sb.append(printForInvalidToken(pos, ident, context))
          }
          case DuplicateIdentifier(pos, ident, context) => {
            sb.append("Duplicate Identifier error in " + filename + " " + "(line " + pos._1 + ", coloumn " + pos._2 + "):\n")
            sb.append(printForInvalidToken(getIdentPos(pos, ident), ident, context))
          }
          case InvalidReturnError(pos, context) => {
            sb.append("Invalid Return Statement error in " + filename + " " + "(line " + pos._1 + ", coloumn " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
            sb.append(printForInvalidToken(pos, "return", None))
          }
          case TypeErasureError(pos, context) => {
            sb.append("Type Erasure error in " + filename + " " + "(line " + pos._1 + ", coloumn " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
            sb.append(printForInvalidToken(pos, fileLines(pos._1 - 1), None))
          }
          case ArityMismatch(pos, expectedArity, foundArity, context) => {
            sb.append("Arity Mismatch error in " + filename + " " + "(line " + pos._1 + ", coloumn " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
            sb.append("Expected number of arguments: " + expectedArity + "\n")
            sb.append("Found number of arguments: " + foundArity + "\n")
            sb.append(printForInvalidToken(pos, fileLines(pos._1 - 1), None))
          }
          case ArrayError(pos, arrName, maxDimension, context) => {
            sb.append("Array Out of Bounds error in " + filename + " " + "(line " + pos._1 + ", coloumn " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
            sb.append("Maximum dimension for " + arrName + ": " + maxDimension + "\n")
            sb.append(printForInvalidToken(pos, arrName, None))
          }
          case TypeError(pos, expectedTypes, foundType, context) => {
            sb.append("Type error in " + filename + " " + "(line " + pos._1 + ", coloumn " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
//            printType(expectedTypes)
//            sb.append("Expected number of arguments: " + expectedArity + "\n")
//            sb.append(printForInvalidToken(pos, arrName, None))
          }

        }
      }
      sb.toString()
    }

    def printForInvalidToken(pos: (Int, Int), token: String, ident: Option[String]): String = {
      val sb = new StringBuilder()
      if (ident.isDefined) {
        sb.append(ident.get +  ": " + token + "\n")
      }
      val numLinesArd = 1

      val errLines: Seq[String] = getLinesAround(pos._1 - 1, numLinesArd, filename, fileLines)

      for (i <- errLines.indices) {
        if (i == numLinesArd) {
          sb.append("| " + errLines(i)+"\n")

          var numCars = token.length
          if ((pos._2 + token.length) > errLines(i).length) {
            numCars = errLines(i).length - pos._2 + 1
          }

          sb.append("| " + (" " * (pos._2-1)) + ("^" * numCars) + "\n")
        } else {
          sb.append("| " + errLines(i) + "\n")
        }
      }
      sb.append("\n\n")
      sb.toString()
    }

  }
}
