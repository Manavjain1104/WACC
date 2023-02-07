package wacc

import org.scalatest.{Pending, Tag}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import java.io.File
import scala.Console.out
import scala.language.postfixOps
import sys.process._
import org.scalatest.matchers.should.Matchers._
import parsley.Failure
import wacc.lexer.{ASSIGN_EQ, fully}
import wacc.parser.program

import scala.io.Source
import scala.util.control.Breaks.break

object ChecksAllValid extends Tag("ChecksAllValid")

class ChecksAllValid extends AnyFlatSpec {

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


        val o = s"./compile $file check" !!

        if (o.contains("100") || o.contains("200")) {
          fail("Wrong exit code")
        }

        //println("echo $?" !!)
//        fully(program).parse(scala.io.Source.fromFile(file).mkString) match {
//          case Failure(_) => ("failed")//sys.exit(100)
//        } //sys.exit(100) }

      }
      case false => Nil
    }

  }

//  behavior of "invalid syntax"
//  it should "print exit code 100" in {
//    val fileContents = Source.fromFile()
//    val text: String = fileContents.getLines().mkString("\n")
//    //println(text)
//    fully(program).parse(text) match {
//      case Success(x) => {
//        //println(s"$x")
//        println("Exit code: 0")
//        //println(x)
//        sys.exit(0)
//      }
//      case Failure(msg) => {
//        println("Syntax error!")
//        //println(msg)
//        println("Exit code: 100")
//        //sys.exit(100)
//      }
//    }
//  }


  behavior of "valid advanced tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/advanced", exampleFn)
  }

  behavior of "valid array tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/array", exampleFn)
  }

  behavior of "valid basic tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/basic", exampleFn)
  }

  behavior of "valid expressions tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/expressions", exampleFn)
  }

  behavior of "valid function tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/function", exampleFn)
  }

  behavior of "valid if tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/if", exampleFn)
  }

  behavior of "valid IO tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/IO", exampleFn)
  }

  behavior of "valid pairs tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/pairs", exampleFn)
  }

  behavior of "valid runtimeErr tests"
  ignore should "succeed with exit code 0" in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/runtimeErr", exampleFn)
  }

  behavior of "valid scope tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/scope", exampleFn)
  }

  behavior of "valid sequence tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/sequence", exampleFn)
  }

  behavior of "valid variables tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/variables", exampleFn)
  }

  behavior of "valid while tests"
  it should "succeed with exit code 0" taggedAs(ChecksAllValid) in {
    applyRecursively("/Users/krishmaha/wacc/new/WACC_25/src/test/scala/wacc/valid/while", exampleFn)
  }


}
