package wacc.backendTests

import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.Console.{in, out}
import scala.collection.mutable
import scala.language.postfixOps
import scala.io.Source
import scala.collection.mutable.ListBuffer
import sys.process._
import scala.util.control.Breaks.break

class ValidTests extends AnyFlatSpec {

//  def applyRecursively(dir: String, fn: (File) => Any) {
//    def listAndProcess(dir: File) {
//      dir.listFiles match {
//        case null => out.println("exception: dir cannot be listed: " + dir.getPath); List[File]()
//        case files => files.toList.sortBy(_.getName).foreach(file => {
//          fn(file)
//          if (!java.nio.file.Files.isSymbolicLink(file.toPath) && file.isDirectory) listAndProcess(file)
//        })
//      }
//    }
//    listAndProcess(new File(dir))
//  }


  //want this to get expected output from .wacc file and compare it to output of our compiler.
  def exampleFn(file: File): String = {
    val source = Source.fromFile(file)
    val lb = ListBuffer[String]()
    val out = ListBuffer[String]()

    for (line <- source.getLines())
      lb.append(line)
    source.close()
    var start = 0
    var end = 0

    for (a <- lb.indices) {
      if (lb(a).startsWith("# Output") && lb(a+1) == "") {
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



    //    println(file.getName)
    //    for (x <- out) {
    //      println(x)
    //    }
    //    println("\n")
    val s = new StringBuilder()
    for (x <- out) {
      s ++= x
    }
    s.toString()

  }



  behavior of "a valid if test"
  it should "succeed with exit code 0" in {
    //    val f = "src/test/scala/wacc/valid/if/if1.wacc"
    //
    //    val s = exampleFn(new File(f))

    //    println(s)
    val directory = new File("src/test/scala/wacc/valid/if")


    if (directory.exists && directory.isDirectory) {
      for (file <- directory.listFiles()) {
        val expectedOutput = exampleFn(file)
        //println(expectedOutput)
        val bashOutput = s"./compile $file" !!

        //println(file)
        //println(expectedOutput)
        //        println(s.toString)
        assert(expectedOutput == bashOutput)
      }
    }







    //    val source = Source.fromFile("src/test/scala/wacc/valid/advanced/binarySortTree.wacc")
    ////
    ////    val stringBuilder = new StringBuilder("")
    //    val lb = ListBuffer[String]()
    //    val out = ListBuffer[String]()
    //
    //    for (line <- source.getLines())
    //      lb.append(line)
    //    source.close()
    //    var start = 0
    //    var end = 0
    //
    //    for (a <- lb.indices) {
    //      if (lb(a).startsWith("# Output")) {
    //        start = a + 1
    //      }
    //      if (lb(a) == "#") {
    //        end = a
    //      }
    //    }
    //    for (a <- start until end) {
    //      out.append(lb(a).drop(2))
    //    }
    //    for (x <- out) {
    //      println(x)
    //    }

    //    lb.sliding(2).foreach(println)




    //    println(lb)
    //    var flag = false
    //
    //    val ite = io.Source.fromFile("src/test/scala/wacc/valid/if/ifTrue.wacc").getLines()
    //
    //    while (ite.hasNext) {
    //      if (ite.next().startsWith("# Output:")) {
    //        flag = true
    //      }
    //      ite.next()
    //      if (flag && ite.hasNext) {
    //        lb.append(ite.next())
    //      }
    //      if (ite.hasNext && ite.next() == "#") {
    //        flag = false
    //      }
    //
    //    }
    //
    //    for (x <- lb) {
    //      println(x)
    //    }






  }



}