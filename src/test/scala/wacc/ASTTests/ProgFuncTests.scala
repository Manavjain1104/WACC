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

    val ptest = Program(List.empty, List(Func(CharType()(0,0),"a",List(Param(CharType()(0,0),"b")(0,0), Param(CharType()(0,0),"c")(0,0),
      Param(CharType()(0,0),"d")(0,0)),Exit(IntExpr(0)(0,0))(0,0))(None, (0,0))),Skip)(0,0)

    val repr = program.parse(prog).get
    assert(repr === ptest)
  }

  behavior of "program2"
  it should "generate the correct output of the program2" taggedAs(ProgFuncTests, SyntacticTests) in {
    val prog = "begin char x (char y, char z, string abc) is return (3*4)%(4/2) end println true && false end"

    val ptest = Program(List.empty, List(Func(CharType()(0,0),"x",List(Param(CharType()(0,0),"y")(0,0),
      Param(CharType()(0,0),"z")(0,0), Param(StringType()(0,0),"abc")(0,0)),
      Return(ModExpr(MulExpr(IntExpr(3)(0,0),IntExpr(4)(0,0))(None,(0,0)),
        DivExpr(IntExpr(4)(0,0),IntExpr(2)(0,0))(None,(0,0)))(None,(0,0)))(0,0))(None, (0,0))),
      Println(AndExpr(BoolExpr(true)(0,0), BoolExpr(false)(0,0))(None,(0,0)))(None,(0,0)))(0,0)

    val repr = program.parse(prog).get
    assert(repr === ptest)
  }


}