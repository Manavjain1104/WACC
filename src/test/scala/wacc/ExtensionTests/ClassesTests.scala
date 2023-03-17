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

object ClassesTests extends Tag("ClassesTests")

class ClassesTests extends AnyFlatSpec {

  def applyRecursively(dir: String, fn: File => Any): Unit = {
    def listAndProcess(dir: File): Unit = {
      dir.listFiles match {
        case null => out.println("exception: dir cannot be listed: " + dir.getPath); List[File]()
        case files => files.toList.sortBy(_.getName).foreach(file => {
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
        val exitCode = lb(a+1).drop(2)
        if (exitCode != "0") {
          assert(true)
        }
      }
    }
  }


  //  behavior of "extension valid classes tests"
  //  it should "succeed with exit code 0" taggedAs (ClassesTests) in {
  //    applyRecursively("src/test/scala/wacc/extensions/classes/validClasses", exampleFn)
  //
  //  }

  behavior of "valid arrayOfClasses class extension test"
  it should "succeed with exit code 0" taggedAs ClassesTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/classes/validClasses/arrayOfClasses.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("21\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid classFunctionality class extension test"
  it should "succeed with exit code 0" taggedAs ClassesTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/classes/validClasses/classFunctionality.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("14\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid classInstantiation class extension test"
  it should "succeed with exit code 0" taggedAs ClassesTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/classes/validClasses/classInstantiation.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("k\n")
    s.append("k\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid pairOfClasses class extension test"
  it should "succeed with exit code 0" taggedAs ClassesTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/classes/validClasses/pairOfClasses.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("79\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid privatePublic class extension test"
  it should "succeed with exit code 0" taggedAs ClassesTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/classes/validClasses/privatePublic.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("14\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid publicDefault class extension test"
  it should "succeed with exit code 0" taggedAs ClassesTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/classes/validClasses/publicDefault.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("14\n")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "valid structInClass class extension test"
  it should "succeed with exit code 0" taggedAs ClassesTests in {
    val bashOutput = s"./compile_and_run src/test/scala/wacc/extensions/classes/validClasses/structInClass.wacc" !!

    val s = new mutable.StringBuilder()
    s.append("")

    if (s.toString() != bashOutput) {
      fail("WRONG OUTPUT")
    }

  }

  behavior of "extension invalid struct tests"
  it should "succeed with exit code 0" taggedAs ClassesTests in {
    applyRecursively("src/test/scala/wacc/extensions/classes/invalidClasses", checkCompileFailure)
  }


}

