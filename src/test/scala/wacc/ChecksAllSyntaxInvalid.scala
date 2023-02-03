package wacc

import java.io.File
import scala.Console.out
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.Console.out
import scala.language.postfixOps
import sys.process._

object ChecksAllSyntaxInvalid extends Tag("ChecksAllSyntaxInvalid")

class ChecksAllSyntaxInvalid extends AnyFlatSpec {

  def applyRecursively(dir: String, fn: (File) => Any) {
    def listAndProcess(dir: File) {
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

  def exampleFn(file: File) {
    //println(s"processing $file")
    //println("pwd" !!)
    //println(s"./compile $file" !!)

    //s"./compile $file"
    //println(s"./")
    file.toString.endsWith(".wacc") match {
      case true => {
        println(s"processing $file")
        println(s"./compile $file test" !!)
        //println("echo $?" !!)
        //        fully(program).parse(scala.io.Source.fromFile(file).mkString) match {
        //          case Failure(_) => ("failed")//sys.exit(100)
        //        } //sys.exit(100) }
      }
      case false => Nil
    }

  }

  behavior of "invalid syntax array tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/array", exampleFn)
  }

  behavior of "invalid syntax basic tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/basic", exampleFn)
  }

  behavior of "invalid syntax expressions tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/expressions", exampleFn)
  }

  behavior of "invalid syntax functions tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/functions", exampleFn)
  }

  behavior of "invalid syntax if tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/if", exampleFn)
  }

  behavior of "invalid syntax literals tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/literals", exampleFn)
  }

  behavior of "invalid syntax pairs tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/pairs", exampleFn)
  }

  behavior of "invalid syntax print tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/print", exampleFn)
  }

  behavior of "invalid syntax sequence tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/sequence", exampleFn)
  }

  behavior of "invalid syntax variables tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/variables", exampleFn)
  }

  behavior of "invalid syntax while tests"
  it should "fail with exit code 100" taggedAs(ChecksAllSyntaxInvalid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/invalid/syntaxErr/while", exampleFn)
  }

}
