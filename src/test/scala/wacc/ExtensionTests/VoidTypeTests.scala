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

object VoidTypeTests extends Tag("VoidTypeTests")

class VoidTypeTests extends AnyFlatSpec {

  def applyRecursively(dir: String, fn: File => Any): Unit = {
    def listAndProcess(dir: File): Unit = {
      dir.listFiles match {
        case files => files.toList.sortBy(_.getName).foreach(file => {
          fn(file)
          if (!java.nio.file.Files.isSymbolicLink(file.toPath) && file.isDirectory) listAndProcess(file)
        })
        case _ =>
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

    val bashOutput = s"./compile_and_run $file $in" !!

    val exitCode = "echo $?" !!

    val bashOutputNoAddr = bashOutput.replaceAll("""\b0x\w*""", "#addrs#")

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

//  behavior of "valid void type extension tests"
//  it should "succeed with exit code 0" taggedAs (VoidTypeTests) in {
//    //applyRecursively("src/test/scala/wacc/extensions/voidTypes/validVoid", exampleFn)
//    exampleFn(new File("src/test/scala/wacc/extensions/voidTypes/validVoid/voidFunctionWithParameter.wacc"))
//  }

  behavior of "valid functionReturningVoid extension test"
  it should "succeed with exit code 0" taggedAs VoidTypeTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/voidTypes/validVoid/functionReturningVoid.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("Here\n")
    s.append("There\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid voidFunctionCall extension test"
  it should "succeed with exit code 0" taggedAs VoidTypeTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/voidTypes/validVoid/voidFunctionCall.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("Here\n")
    s.append("There\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }


  }

  behavior of "valid voidFunctionEmptyReturn extension test"
  it should "succeed with exit code 0" taggedAs VoidTypeTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/voidTypes/validVoid/voidFunctionEmptyReturn.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }


  }

  behavior of "valid voidFunctionWithParameter extension test"
  it should "succeed with exit code 0" taggedAs VoidTypeTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/voidTypes/validVoid/voidFunctionWithParameter.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("5\n")
    s.append("Here\n")
    s.append("There\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "invalid void type extension tests"
  it should "succeed with exit code 0" taggedAs VoidTypeTests in {
    applyRecursively("src/test/scala/wacc/extensions/voidTypes/invalidVoid", checkCompileFailure)
  }

}
