package wacc.ASTTests

import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import wacc.AST._
import wacc.parser._

object ProgFuncTests extends Tag("ProgFuncTests")

class ProgFuncTests extends AnyFlatSpec {

  behavior of "program"
  it should "generate the correct output of the program" taggedAs(ProgFuncTests, SyntacticTests) in {
    val prog = "begin char a (char b, char c, char d) is exit 0 end skip end"

    val ptest = Program(List(Func(CharType,"a",List(Param(CharType,"b"), Param(CharType,"c"),
      Param(CharType,"d")),Exit(IntExpr(0)))),Skip)

    val repr = program.parse(prog).get
    assert(repr === ptest)
  }

  behavior of "program2"
  it should "generate the correct output of the program2" taggedAs(ProgFuncTests, SyntacticTests) in {
    val prog = "begin char x (char y, char z, string abc) is return (3*4)%(4/2) end println true && false end"

    val ptest = Program(List(Func(CharType,"x",List(Param(CharType,"y"), Param(CharType,"z"), Param(StringType,"abc")),
      Return(ModExpr(MulExpr(IntExpr(3),IntExpr(4)),DivExpr(IntExpr(4),IntExpr(2)))))),Println(AndExpr(BoolExpr(true),
      BoolExpr(false))))

    val repr = program.parse(prog).get
    assert(repr === ptest)
  }
}