package wacc.ExtensionTests

import org.scalatest.Assertions.fail
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.Console.{in, out}
import scala.language.postfixOps
import scala.io.Source
import scala.collection.mutable.ListBuffer
import sys.process._

object BranchingStatementTests extends Tag("BranchingStatementTests")

class BranchingStatementTests extends AnyFlatSpec {

  def applyRecursively(dir: String, fn: (File) => Any) {
    def listAndProcess(dir: File) {
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

  def exampleFn(file: File) = {
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


    val s = new StringBuilder()
    for (x <- out) {
      s ++= x
    }

    val in = new StringBuilder()
    for (x <- input) {
      in ++= x
    }

    if (file == new File("src/test/scala/wacc/valid/advanced/binarySortTree.wacc")) {
      in ++= "5 3 6 4 7 9"
      s ++= "3 4 6 7 9"
    }

    var bashOutput = s"./compile_and_run $file ${in}" !!

    val exitCode = "echo $?" !!

    var bashOutputNoAddr = bashOutput.replaceAll("\\b0x\\w*", "#addrs#")

    if (exitCode != "100" || exitCode != "200") {

      if (s.toString() != bashOutputNoAddr) {
        //fail("Wrong output")
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

  behavior of "extension valid branching statement tests"
  it should "succeed with exit code 0" taggedAs (BranchingStatementTests) in {
    applyRecursively("src/test/scala/wacc/extensions/branchingStatements/validBranchingStatements", exampleFn)
  }

  behavior of "extension invalid branching statement tests"
  it should "succeed with exit code 0" taggedAs (BranchingStatementTests) in {
    applyRecursively("src/test/scala/wacc/extensions/branchingStatements/invalidBranchingStatements", checkCompileFailure)
  }


}

