package wacc

import wacc.SemTypes._
import wacc.error._

import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource
import scala.io.Source._

object printer {

  final val SYNTAX_ERROR_CODE = 100
  final val SEMANTIC_ERROR_CODE = 200
  final val OK_EXIT_CODE = 0

  def generateOutputMessages(semanticErrLogs: ListBuffer[SemanticError],
                             syntaxError: Option[SyntaxError],
                             filename: String, exitCode: Int): Unit = {
    if (exitCode == OK_EXIT_CODE) {
      println("---- Compilation success. Exit code 0 returned ---- \n")
      return
    }

    val sb = new StringBuilder()

    if (exitCode == SYNTAX_ERROR_CODE) {
      sb.append("---- Syntax Error found. Exit code 100 returned ---- \n")
      sb.append(SyntaxPrinter(syntaxError.get, filename).print())
      println(sb.toString())
      return
    }

    // semantic error(s) case
    sb.append(SemanticPrinter(semanticErrLogs, filename).print())
    println(sb.toString())
  }

  def getLinesAround(lineNum: Int, noOfLines: Int, lineFromFile: List[String]): List[String] = {
    val out = collection.mutable.ListBuffer.empty[String]
    for (i <- (math.max(0, lineNum - noOfLines) to math.min(lineFromFile.length - 1, lineNum + noOfLines))) {
      out += lineFromFile(i)
    }
    out.toList
  }


  sealed trait Printer {
    def print(): String
  }

  case class SemanticPrinter(errorLog: ListBuffer[SemanticError], filename: String) extends Printer {

    // initial constructor code
    val source: BufferedSource = fromFile(filename)
    val fileLines: List[String] = source.getLines().map(_.replace("\t", "    ")).toList
    source.close()

    override def print(): String = {
      val sb = new StringBuilder
      for (error <- errorLog) {
        error match {
          case UnknownIdentifierError(pos, ident, context) => {
            val newPos = getIdentPos(pos, ident)
            sb.append("Unknown Identifier error in " + filename + " " + "(line " + newPos._1 + ", column " + newPos._2 + "):\n")
            sb.append(printForInvalidToken(newPos, ident, context))
          }
          case DuplicateIdentifier(pos, ident, context) => {
            val newPos = getIdentPos(pos, ident)
            sb.append("Duplicate Identifier error in " + filename + " " + "(line " + newPos._1 + ", column " + newPos._2 + "):\n")
            sb.append(printForInvalidToken(newPos, ident, context))
          }
          case InvalidReturnError(pos, context) => {
            sb.append("Invalid Return Statement error in " + filename + " " + "(line " + pos._1 + ", column " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
            sb.append(printForInvalidToken(pos, "return", None))
          }
          case TypeErasureError(pos, context) => {
            sb.append("Type Erasure error in " + filename + " " + "(line " + pos._1 + ", column " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
            sb.append(printForInvalidToken(pos, fileLines(pos._1 - 1), None))
          }
          case ArityMismatch(pos, expectedArity, foundArity, context) => {
            sb.append("Arity Mismatch error in " + filename + " " + "(line " + pos._1 + ", column " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
            sb.append("Expected number of arguments: " + expectedArity + "\n")
            sb.append("Found number of arguments: " + foundArity + "\n")
            sb.append(printForInvalidToken(pos, fileLines(pos._1 - 1), None))
          }
          case ArrayError(pos, arrName, maxDimension, context) => {
            sb.append("Array Out of Bounds error in " + filename + " " + "(line " + pos._1 + ", column " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
            sb.append("Maximum dimension for " + arrName + ": " + maxDimension + "\n")
            sb.append(printForInvalidToken(pos, arrName, None))
          }

          case TypeError(pos, expectedTypes, foundType, context) => {
            sb.append("Type error in " + filename + " " + "(line " + pos._1 + ", column " + pos._2 + "):\n")
            if (context.isDefined) {
              sb.append(context.get + "\n")
            }
            sb.append(printForTypeError(pos, expectedTypes.toList, foundType))
          }

        }
      }
      sb.toString()
    }

    def printForInvalidToken(pos: (Int, Int), token: String, ident: Option[String]): String = {
      val sb = new StringBuilder()
      if (ident.isDefined) {
        sb.append(ident.get + ": " + token + "\n")

      }
      val numLinesArd = 1

      val errLines: Seq[String] = getLinesAround(pos._1 - 1, numLinesArd, fileLines)

      for (i <- errLines.indices) {
        if (i == numLinesArd) {
          sb.append("| " + errLines(i) + "\n")
          var numCars = token.length
          if ((pos._2 + token.length) > errLines(i).length) {
            numCars = errLines(i).length - pos._2 + 1
          }

          sb.append("| " + (" " * (pos._2 - 1)) + ("^" * numCars) + "\n")
        } else {
          sb.append("| " + errLines(i) + "\n")
        }
      }
      sb.append("\n\n")
      sb.toString()
    }

    def printForTypeError(pos: (Int, Int), expectedTypes: List[SemType], foundType: SemType): String = {
      val sb = new StringBuilder()
      val foundTypeString = typeToString(foundType)
      sb.append("unexpected : " + foundTypeString + "\n")
      sb.append("expected   : ")
      if (expectedTypes.nonEmpty) {
        sb.append(typeToString(expectedTypes.head))
        for (expectedType <- expectedTypes.tail) {
          sb.append("  " + typeToString(expectedType))
        }
      }
      sb.append("\n")

      sb.append(printForInvalidToken(pos, getTillNextToken(pos), None))
      sb.toString()
    }

    def typeToString(foundType: SemType): String = {
      foundType match {
        case IntSemType => "int"
        case BoolSemType => "bool"
        case CharSemType => "char"
        case StringSemType => "string"
        case PairSemType(pt1, pt2) => {
          if (pt1 == InternalPairSemType && pt2 == InternalPairSemType) {
            "pair type"
          } else {
            "pair (" + typeToString(pt1) + ", " + typeToString(pt2) + ")"
          }
        }
        case ArraySemType(t: SemType) => {
          if (t == InternalPairSemType) {
            "array type"
          } else {
            "array [ " + typeToString(t) + " ]"
          }
        }
        case InternalPairSemType => "pair"
        case FuncSemType(retType, paramTypes, _) => {
          val sb = new StringBuilder()
          sb.append("Function: ")
          for (paramType <- paramTypes) {
            sb.append(typeToString(paramType) + " -> ")
          }
          sb.append(typeToString(retType) + "\n")
          sb.toString()
        }
      }
    }

    def getIdentPos(pos: (Int, Int), ident: String): (Int, Int) = {
      var line = pos._1 - 1
      var col = pos._2 - 1
      val len = ident.length
      while (line < fileLines.length && !fileLines(line).slice(col, col + len).equals(ident)) {
        col += 1
        if (col >= fileLines(line).length || fileLines(line).charAt(col) == '\n') {
          col = 0
          line += 1
        }
      }
      (line + 1, col + 1)
    }

    def getTillNextToken(pos: (Int, Int)): String = {
      var col = pos._2 - 1
      var line = pos._1 - 1
      var regex = """ ;="""
      val sb = new StringBuilder()
      while (line < fileLines.length &&
        col < fileLines(line).length &&
        fileLines(line).nonEmpty &&
        !regex.contains(fileLines(line).charAt(col))) {
        val c = fileLines(line).charAt(col)
        sb.append(c)
        if ("[(\'\"".contains(c)) {
          sb.append("_")
          regex = regex.replace(" ", "") + c
        }
        col += 1
        if (col >= fileLines(line).length || fileLines(line).charAt(col) == '\n') {
          col = 0
          line += 1
        }
      }
      if (sb.isEmpty) {
        return ""
      }
      sb.toString()
    }

  }

  case class SyntaxPrinter(syntaxError: SyntaxError, filename: String) extends Printer {

    override def print(): String = {
      val sb = new StringBuilder()
      sb.append("Syntax error in " + filename + " " + "(line " + syntaxError.pos._1 + ", column " + syntaxError.pos._2 + "):\n")

      syntaxError.lines match {
        case VanillaError(unexpected, expected, reasons, line) => {

          // printing reasons
          if (reasons.nonEmpty) {
            sb.append("Help:-\n")
            for (reason <- reasons) {
              sb.append(reason + "\n")
            }
          }

          if (unexpected.isDefined) {
            sb.append("Unexpected : ")
            sb.append(printItem(unexpected.get))
            sb.append("\n")
          }

          if (expected.nonEmpty) {
            sb.append("Expected   : ")
            sb.append(printItem(expected.head))
            for (item <- expected.tail) {
              sb.append("  " + printItem(item))
            }
            sb.append("\n")
          }

          // printing
          sb.append(printLine(line))
        }

        case SpecialisedError(msgs, line) => {

          // printing error
          sb.append("Context:- \n")
          for (msg <- msgs) {
            sb.append(msg + "\n")
          }
          sb.append(printLine(line))
        }
      }
      sb.append("\n\n")
      //      println(sb)
      sb.toString()
    }

    def printLine(line: LineInfo): String = {
      val sb = new StringBuilder()
      for (lineBefore <- line.linesBefore) {
        sb.append("| " + lineBefore + "\n")
      }
      sb.append("| " + line.line + "\n")
      sb.append("| " + (" " * line.errorPointsAt) + "^" * math.min(line.errorWidth, (line.line.length - line.errorPointsAt)) + "\n")
      for (lineAfter <- line.linesAfter) {
        sb.append("| " + lineAfter + "\n")
      }
      sb.toString()
    }

    def printItem(item: SyntaxErrorItem): String = {
      item match {
        case SyntaxRaw(item) => item
        case SyntaxNamed(item) => item
        case error.SyntaxEndOfInput => "End of input"
      }
    }

  }


}
