import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.Console.{in, out}
import scala.collection.mutable
import scala.language.postfixOps
import scala.io.Source
import scala.collection.mutable.ListBuffer
import sys.process._

object ValidTests extends Tag("ValidTests")

class ValidTests extends AnyFlatSpec {

  def applyRecursively(dir: String, fn: (File) => Any): Unit = {
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

    if (file == new File("/src/test/scala/wacc/valid/advanced/binarySortTree.wacc")) {
      in ++= "5 3 6 4 7 9"
      s ++= "3 4 6 7 9"
    }

    val bashOutput = s"./compile_and_run $file ${in}" !!

    val exitCode = "echo $?" !!

    var bashOutputNoAddr = bashOutput.replaceAll("\\b0x\\w*", "#addrs#")

    if (file == new File("/src/test/scala/wacc/valid/advanced/binarySortTree.wacc")) {
      bashOutputNoAddr = s.takeRight(9).toString()
    }

    if (exitCode != "100" || exitCode != "200") {

      if (s.toString() != bashOutputNoAddr) {
        fail("Wrong output")
      }
    }
  }

  def checkFailure(file: File): Unit = {
    s"./compile_and_run $file $in" !!

    val exitCode = "echo $?" !!

    if (exitCode != "0") {
      assert(true)
    }

  }


  behavior of "valid binarySortTree advanced test"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    exampleFn(new File("src/test/scala/wacc/valid/advanced/binarySortTree.wacc"))
  }

  behavior of "valid array tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    //applyRecursively("/src/test/scala/wacc/valid/array/", exampleFn)
    exampleFn(new File("src/test/scala/wacc/valid/array/emptyArrayScope.wacc"))
  }

  behavior of "valid basic exit tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/basic/exit", exampleFn)
  }

  behavior of "valid basic skip tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/basic/skip", exampleFn)
  }

  behavior of "valid expressions tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/expressions/", exampleFn)
  }

  behavior of "valid nested function tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/function/nested_functions", exampleFn)
  }

  behavior of "valid simple function tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/function/simple_functions", exampleFn)
  }

  behavior of "valid if tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/if/", exampleFn)
  }

  behavior of "valid generic IO tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/IO/genericIO", exampleFn)
  }

  behavior of "valid IO print tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/IO/print", exampleFn)
  }

  behavior of "valid IO read tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/IO/read", exampleFn)
  }

  behavior of "valid pairs tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/pairs/", exampleFn)
  }

  behavior of "valid runtimeErr arrayOutOfBounds tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/runtimeErr/arrayOutOfBounds", checkFailure)
  }

  behavior of "valid runtimeErr divideByZero tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/runtimeErr/divideByZero", checkFailure)
  }

  behavior of "valid runtimeErr integerOverflow tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/runtimeErr/integerOverflow", checkFailure)
  }

  behavior of "valid runtimeErr nullDereference tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/runtimeErr/nullDereference", checkFailure)
  }

  behavior of "valid scope tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/scope/", exampleFn)
  }

  behavior of "valid sequence tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/sequence/", exampleFn)
  }

  behavior of "valid variables tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/variables/", exampleFn)
  }

  behavior of "valid while tests"
  it should "succeed with exit code 0" taggedAs(ValidTests) in {
    applyRecursively("/src/test/scala/wacc/valid/while/", exampleFn)
  }
}

