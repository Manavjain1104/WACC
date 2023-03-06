//package wacc
//
//import java.io.File
//import scala.Console.out
//
//import org.scalatest.{Tag}
//import org.scalatest.flatspec.AnyFlatSpec
//import scala.language.postfixOps
//import sys.process._
//
//object ChecksAllSemanticInvalid extends Tag("ChecksAllSemanticInvalid")
//
//class ChecksAllSemanticInvalid extends AnyFlatSpec {
//
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
//
//
//  def exampleFn(file: File) {
//    file.toString.endsWith(".wacc") match {
//      case true => {
//        println(s"processing $file")
//        val o = s"./compileFrontend.sh $file check" !!
//
//        println(o.takeRight(4))
//        if (o.contains("100") || o.contains(" 0")) {
//          fail("Wrong exit code")
//        }
//      }
//      case false => Nil
//    }
//
//  }
//
//  behavior of "invalid semantic array tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSyntaxInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/array", exampleFn)
//
//  }
//
//  behavior of "invalid semantic exit tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/exit", exampleFn)
//  }
//
//  behavior of "invalid semantic expressions tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/expressions", exampleFn)
//  }
//
//  behavior of "invalid semantic functions tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/function", exampleFn)
//  }
//
//  behavior of "invalid semantic if tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/if", exampleFn)
//  }
//
//  behavior of "invalid semantic IO tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/IO", exampleFn)
//  }
//
//  behavior of "invalid semantic multiple tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/multiple", exampleFn)
//  }
//
//  behavior of "invalid semantic pairs tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/pairs", exampleFn)
//  }
//
//  behavior of "invalid semantic print tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/print", exampleFn)
//  }
//
//  behavior of "invalid semantic read tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/read", exampleFn)
//  }
//
//  behavior of "invalid semantic scope tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/scope", exampleFn)
//  }
//
//  behavior of "invalid semantic variables tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/variables", exampleFn)
//  }
//
//  behavior of "invalid semantic while tests"
//  it should "fail with exit code 100" taggedAs (ChecksAllSemanticInvalid) in {
//    applyRecursively("/src/test/scala/wacc/invalid/semanticErr/while", exampleFn)
//  }
//
//}
