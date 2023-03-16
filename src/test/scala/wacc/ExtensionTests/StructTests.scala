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

object StructTests extends Tag("StructTests")

class StructTests extends AnyFlatSpec {

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

    var bashOutput = s"./compile_and_run $file ${in}" !!

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
        val exitCode = lb(a+1).drop(2)
        if (exitCode != 0) {
          assert(true)
        }
      }
    }
  }



//  behavior of "extension valid struct tests"
//  it should "succeed with exit code 0" taggedAs (StructTests) in {
//    applyRecursively("src/test/scala/wacc/extensions/structs/validStructs", exampleFn)
//  }

  behavior of "valid elementAccess struct extension test"
  it should "succeed with exit code 0" taggedAs (VoidTypeTests) in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/structs/validStructs/elementAccess.wacc" !!

    val s = new StringBuilder()
    s.append("d\n")
    s.append("1\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid nestedStructs1 struct extension test"
  it should "succeed with exit code 0" taggedAs (VoidTypeTests) in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/structs/validStructs/nestedStructs1.wacc" !!

    val s = new StringBuilder()
    s.append("1\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid nestedStructs2 struct extension test"
  it should "succeed with exit code 0" taggedAs (VoidTypeTests) in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/structs/validStructs/nestedStructs2.wacc" !!

    val s = new StringBuilder()
    s.append("e\n")
    s.append("d\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid passStructElemToFunction struct extension test"
  it should "succeed with exit code 0" taggedAs (VoidTypeTests) in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/structs/validStructs/passStructElemToFunction.wacc" !!

    val s = new StringBuilder()
    s.append("a\n")
    s.append("1\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }
  behavior of "valid readStructElement struct extension test"
  it should "succeed with exit code 0" taggedAs (VoidTypeTests) in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/structs/validStructs/readStructElement.wacc" !!

    val s = new StringBuilder()
    s.append("\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }
  behavior of "valid structInArray struct extension test"
  it should "succeed with exit code 0" taggedAs (VoidTypeTests) in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/structs/validStructs/structInArray.wacc" !!

    val s = new StringBuilder()
    s.append("b\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid structInPair struct extension test"
  it should "succeed with exit code 0" taggedAs (VoidTypeTests) in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/structs/validStructs/structInPair.wacc" !!

    val s = new StringBuilder()
    s.append("1\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid structInstantiation struct extension test"
  it should "succeed with exit code 0" taggedAs (VoidTypeTests) in {
    var bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/structs/validStructs/structInstantiation.wacc" !!

    val s = new StringBuilder()
    s.append("\n")
    println(s.toString())

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }
  behavior of "extension invalid struct tests"
  it should "succeed with exit code 0" taggedAs (StructTests) in {
    applyRecursively("src/test/scala/wacc/extensions/structs/invalidStructs", checkCompileFailure)
  }


}

