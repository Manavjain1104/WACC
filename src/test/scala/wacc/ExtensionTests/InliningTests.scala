package wacc.ExtensionTests

import org.scalatest.Assertions.fail
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.Console.out
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.language.postfixOps
import sys.process._

object InliningTests extends Tag("InliningTests")
class InliningTests extends AnyFlatSpec {

  def applyRecursively(lookFor: String, dir: String, fn: (String, File) => Any): Unit = {
    def listAndProcess(dir: File): Unit = {
      dir.listFiles match {
        case null => out.println("exception: dir cannot be listed: " + dir.getPath); List[File]()
        case files => files.toList.sortBy(_.getName).foreach(file => {
          println(s"processing $file")
          fn(lookFor, file)
          if (!java.nio.file.Files.isSymbolicLink(file.toPath) && file.isDirectory) listAndProcess(file)
        })
      }

    }

    listAndProcess(new File(dir))
  }

  def checkInlinable(lookFor: String, file: File): Unit = {
    s"./compile $file" !!

    val assemblyFile = new File(file.getName.dropRight(4) + "s")

    for (line <- Source.fromFile(assemblyFile).getLines) {
      if (line.contains(lookFor)) {
        fail("Should be inlinable but isn't")
      }
    }
  }

  def checkNonInlinable(lookFor: String, file: File): Unit = {
    s"./compile $file" !!

    val assemblyFile = new File(file.getName.dropRight(4) + "s")

    var contains = false
    for (line <- Source.fromFile(assemblyFile).getLines) {
      if (line.contains(lookFor)) {
        contains = true
      }
    }

    if (!contains) {
      fail("Should be non-inlinable but is inlinable")
    }
  }

  behavior of "extension inlinable test 1"
  it should "succeed with exit code 0" taggedAs (InliningTests) in {
    checkInlinable("neg", new File("src/test/scala/wacc/extensions/inlining/inlinable/inlinable1.wacc"))
  }

  behavior of "extension inlinable test 2"
  it should "succeed with exit code 0" taggedAs (InliningTests) in {
    checkInlinable("inc", new File("src/test/scala/wacc/extensions/inlining/inlinable/inlinable2.wacc"))
  }

  behavior of "extension noninlinable test"
  it should "succeed with exit code 0" taggedAs (InliningTests) in {
    checkNonInlinable("rec", new File("src/test/scala/wacc/extensions/inlining/nonInlinable/nonInlinable.wacc"))
  }

  behavior of "extension too long to inline test"
  it should "succeed with exit code 0" taggedAs (InliningTests) in {
    checkNonInlinable("wacc_f", new File("src/test/scala/wacc/extensions/inlining/nonInlinable/tooLong.wacc"))
  }

}
