
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.Console.{in, out}
import scala.collection.mutable
import scala.language.postfixOps
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.reflect.io.Directory
import sys.process._
import scala.util.control.Breaks.break

class ValidTests extends AnyFlatSpec {

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


  //want this to get expected output from .wacc file and compare it to output of our compiler.
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





    //    println(file.getName)
    //    for (x <- out) {
    //      println(x)
    //    }
    //    println("\n")
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

    //println(bashOutput)

    /*
    for (line <- s.toString().split("\n")) {
      if (line.contains("#addrs#")) {
        println(line)
        val l = line.replaceAll("#addrs#", "0x")

      }
    }
     */

    /*var counter = 0
    for (char <- s.toString()) {
      if (char == '#') {
        s.replace(counter, counter + 8, "0x...")
      }
      counter = counter + 1
    }*/

    val bashOutputNoAddr = bashOutput.replaceAll("\\b0x\\w*", "#addrs#")

    println(bashOutputNoAddr)
    println(s.toString())




    if (exitCode != "100" || exitCode != "200") {

      if (s.toString() != bashOutputNoAddr) {
        fail("Wrong output")
      }
    }
    //val bashOutput = "correct\n"

    //println(s.toString())
    //println(s"bash output: $bashOutput")



  }

  behavior of "valid advanced tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/advanced/", exampleFn)
  }

  behavior of "valid array tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/array/", exampleFn)
  }

  behavior of "valid basic exit tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/basic/exit", exampleFn)
  }

  behavior of "valid basic skip tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/basic/skip", exampleFn)
  }

  behavior of "valid expressions tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/expressions/", exampleFn)
  }

  behavior of "valid nested function tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/function/nested_functions", exampleFn)
  }

  behavior of "valid simple function tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/function/simple_functions", exampleFn)
  }

  behavior of "valid if tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/if/", exampleFn)
  }

  behavior of "valid IO tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/IO/", exampleFn)
  }

  behavior of "valid IO print tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/IO/print", exampleFn)
  }

  behavior of "valid IO read tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/IO/read", exampleFn)
  }

  behavior of "valid pairs tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/pairs/", exampleFn)
  }

  behavior of "valid runtimeErr arrayOutOfBounds tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/runtimeErr/arrayOutOfBounds", exampleFn)
  }

  behavior of "valid runtimeErr divideByZero tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/runtimeErr/divideByZero", exampleFn)
  }

  behavior of "valid runtimeErr integerOverflow tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/runtimeErr/integerOverflow", exampleFn)
  }

  behavior of "valid runtimeErr nullDereference tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/runtimeErr/nullDereference", exampleFn)
  }

  behavior of "valid scope tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/scope/", exampleFn)
  }

  behavior of "valid sequence tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/sequence/", exampleFn)
  }

  behavior of "valid variables tests" //FINE
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/variables/", exampleFn)
  }

  behavior of "valid while tests"
  it should "succeed with exit code 0" in {
    applyRecursively("src/test/scala/wacc/valid/while/", exampleFn)
  }
}




