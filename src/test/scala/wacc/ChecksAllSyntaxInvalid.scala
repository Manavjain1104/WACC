package wacc

import java.io.File
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.postfixOps
import sys.process._

object ChecksAllSyntaxInvalid extends Tag("ChecksAllSyntaxInvalid")

class ChecksAllSyntaxInvalid extends AnyFlatSpec {

  def applyRecursively(dir: String, fn: (File) => Any): Unit = {
    def listAndProcess(dir: File): Unit = {
      dir.listFiles match {
        case null =>
        case files => files.toList.sortBy(_.getName).foreach(file => {
          fn(file)
          if (!java.nio.file.Files.isSymbolicLink(file.toPath) && file.isDirectory) listAndProcess(file)
        })
      }
    }
    listAndProcess(new File(dir))
  }

  def exampleFn(file: File): Unit = {
    file.toString.endsWith(".wacc") match {
      case true => {
        println(s"processing $file")
        val o = s"./compileFrontend.sh $file check" !!

        println(s"file: $o")
        if (o.contains(" 0") || o.contains("200")) {
          fail("Wrong exit code")
        }
      }
      case false =>
    }
  }

  def getListOfFiles(dir: String): List[String] = {
    val file = new File(dir)
    file.listFiles.filter(_.isFile)
      .filter(_.getName.endsWith(".wacc"))
      .map(_.getPath).toList
  }

  behavior of "invalid syntax array tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/array", exampleFn)

  }

  behavior of "invalid syntax basic tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/basic", exampleFn)
  }

  behavior of "invalid syntax expressions tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/expressions", exampleFn)
  }

  behavior of "invalid syntax functions tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/functions", exampleFn)
  }

  behavior of "invalid syntax if tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/if", exampleFn)
  }

  behavior of "invalid syntax literals tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/literals", exampleFn)
  }

  behavior of "invalid syntax pairs tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/pairs", exampleFn)
  }

  behavior of "invalid syntax print tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/print", exampleFn)
  }

  behavior of "invalid syntax sequence tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/sequence", exampleFn)
  }

  behavior of "invalid syntax variables tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/variables", exampleFn)
  }

  behavior of "invalid syntax while tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/src/test/scala/wacc/invalid/syntaxErr/while", exampleFn)
  }

}
