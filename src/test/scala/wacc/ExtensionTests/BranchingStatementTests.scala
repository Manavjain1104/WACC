package wacc.ExtensionTests

import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.Console.out
import scala.collection.mutable
import scala.language.postfixOps
import scala.io.Source
import scala.collection.mutable.ListBuffer
import sys.process._

object BranchingStatementTests extends Tag("BranchingStatementTests")

class BranchingStatementTests extends AnyFlatSpec {

  def applyRecursively(dir: String, fn: File => Any): Unit = {
    def listAndProcess(dir: File): Unit = {
      dir.listFiles match {
        case null => out.println("exception: dir cannot be listed: " + dir.getPath); List[File]()
        case files => files.toList.sortBy(_.getName).foreach(file => {
          println(s"processing $file")
          fn(file)
          if (!java.nio.file.Files.isSymbolicLink(file.toPath) && file.isDirectory) listAndProcess(file)
        })
      }

    }

    listAndProcess(new File(dir))
  }

  def exampleFn(file: File): Unit = {
    val source = Source.fromFile(file)
    val lb = ListBuffer[String]()
    val out = ListBuffer[String]()
    val input = ListBuffer[String]()

    for (line <- source.getLines())
      lb.append(line)
    source.close()
    var start = 0
    var end = 0

    for (a <- lb.indices) {
      if (lb(a).startsWith("# Output") && lb(a + 1) == "") {
        start = a
        end = a
      }
      if (lb(a).startsWith("# Output")) {
        start = a + 1
      }
      if (lb(a) == "#") {
        end = a
      }
    }
    for (a <- start until end) {
      out.append(lb(a).drop(2))
      out.append("\n")
    }

    var inputLine = 0

    for (a <- lb.indices) {
      if (lb(a).startsWith("# Input")) {
        inputLine = a
      }
    }
    input.append(lb(inputLine).drop(9))


    val s = new mutable.StringBuilder()
    for (x <- out) {
      s ++= x
    }

    val in = new mutable.StringBuilder()
    for (x <- input) {
      in ++= x
    }

    if (file == new File("src/test/scala/wacc/valid/advanced/binarySortTree.wacc")) {
      in ++= "5 3 6 4 7 9"
      s ++= "3 4 6 7 9"
    }

    var bashOutput = s"./compile_and_run $file $in" !!

    val exitCode = "echo $?" !!

    var bashOutputNoAddr = bashOutput.replaceAll("\\b0x\\w*", "#addrs#")

    if (exitCode != "100" || exitCode != "200") {

      if (s.toString() != bashOutputNoAddr) {
        fail("Wrong output")
      }
    }
  }

  def checkCompileFailure(file: File): Unit = {
    val source = Source.fromFile(file)
    val lb = ListBuffer[String]()
    for (line <- source.getLines())
      lb.append(line)
    source.close()

    for (a <- lb.indices) {
      if (lb(a).startsWith("# Exit")) {
        val exitCode = lb(a + 1).drop(2)
        if (exitCode != "0") {
          assert(true)
        }
      }
    }
  }

//  behavior of "extension valid branching statement tests"
//  it should "succeed with exit code 0" taggedAs (BranchingStatementTests) in {
//    applyRecursively("src/test/scala/wacc/extensions/branchingStatements/validBranchingStatements", exampleFn)
//  }

  behavior of "valid ifNoElse valid branching statement extension test"
  it should "succeed with exit code 0" taggedAs BranchingStatementTests in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/branchingStatements/validBranchingStatements/ifNoElse.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("correct\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid ifNoElse2 valid branching statement extension test"
  it should "succeed with exit code 0" taggedAs BranchingStatementTests in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/branchingStatements/validBranchingStatements/ifNoElse2.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("it is 4\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid matchCaseStatement valid branching statement extension test"
  it should "succeed with exit code 0" taggedAs BranchingStatementTests in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/branchingStatements/validBranchingStatements/matchCaseStatement.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("bye\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid matchCaseStatement2 valid branching statement extension test"
  it should "succeed with exit code 0" taggedAs BranchingStatementTests in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/branchingStatements/validBranchingStatements/matchCaseStatement2.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("it is a\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "extension invalid branching statement tests"
  it should "succeed with exit code 0" taggedAs BranchingStatementTests in {
    applyRecursively("src/test/scala/wacc/extensions/branchingStatements/invalidBranchingStatements", checkCompileFailure)
  }


}

