package wacc

import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.Console.out
import scala.language.postfixOps
import sys.process._

object ChecksAllValid extends Tag("ChecksAllValid")

class ChecksAllValid extends AnyFlatSpec {

  def applyRecursively(dir: String, fn: (File) => Any): Unit = {
    def listAndProcess(dir: File): Unit = {
      dir.listFiles match {
        case null => out.println(s"Processing $dir ...")
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
    file.toString.endsWith(".wacc") match {
      case true => {
        println(s"processing $file")
        val o = s"./compileFrontend.sh $file check" !!

        if (o.contains("returning 100") || o.contains("returning 200")) {
          fail("Wrong exit code")
        }
      }
      case false =>
    }
  }

  behavior of "valid advanced tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    println("ls" !!)
    println("pwd" !!)

    applyRecursively("/src/test/scala/wacc/valid/advanced", exampleFn)
  }

  behavior of "valid array tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/array", exampleFn)
  }

  behavior of "valid basic tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/basic", exampleFn)
  }

  behavior of "valid expressions tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/expressions", exampleFn)
  }

  behavior of "valid function tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/function", exampleFn)
  }

  behavior of "valid if tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/if", exampleFn)
  }

  behavior of "valid IO tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/IO", exampleFn)
  }

  behavior of "valid pairs tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/pairs", exampleFn)
  }

  behavior of "valid runtimeErr tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/runtimeErr", exampleFn)
  }

  behavior of "valid scope tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/scope", exampleFn)
  }

  behavior of "valid sequence tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/sequence", exampleFn)
  }

  behavior of "valid variables tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/variables", exampleFn)
  }

  behavior of "valid while tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/src/test/scala/wacc/valid/while", exampleFn)
  }

}
