package wacc.ASTTests

import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import wacc.AST.{ArrayElem, Fst, IdentValue, IntExpr, Snd}
import wacc.SemTypes.InternalPairSemType
import wacc.parser._

object LValueTests extends Tag("LValueTests")

class LValueTests extends AnyFlatSpec {

  behavior of "ident LValue"
  it should "generate the correct ident LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "abcde"
    val lvalue_ident = IdentValue(exp)(None, (0,0))
    val repr = lvalue.parse(exp).get
    assert(repr === lvalue_ident)
  }

  behavior of "array-elem LValue"
  it should "generate the correct single array-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "arr[3]"
    val array_elem_1 = ArrayElem("arr",List(IntExpr(3)(0,0)))(None, (0,0))
    val repr = lvalue.parse(exp).get
    assert(repr === array_elem_1)
  }

  behavior of "array-elem LValue"
  it should "generate the correct multiple array-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "arr[3][4][5]"
    val array_elem_2 = ArrayElem("arr", List(IntExpr(3)(0,0), IntExpr(4)(0,0), IntExpr(5)(0,0)))(None, (0,0))
    val repr = lvalue.parse(exp).get
    assert(repr === array_elem_2)
  }

  behavior of "ident pair-elem LValue"
  it should "generate the correct ident pair-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "fst abcde"
    val pair_elem_ident = new Fst(IdentValue("abcde")(None, (0,0)))(0,0)(InternalPairSemType)
    val repr = lvalue.parse(exp).get
    assert(repr === pair_elem_ident)
  }

  behavior of "array-elem pair-elem LValue"
  it should "generate the correct array-elem pair-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "snd arr[1][2]"
    val pair_elem_array_elem = new Snd(ArrayElem("arr",List(IntExpr(1)(0,0), IntExpr(2)(0,0)))(None, (0,0)))(0,0)(InternalPairSemType)
    val repr = lvalue.parse(exp).get
    assert(repr === pair_elem_array_elem)
  }

  behavior of "nested pair-elem LValue"
  it should "generate the correct nested pair-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "fst snd abcde"
    val nested_pair_elem = new Fst(new Snd(IdentValue("abcde")(None, (0,0)))(0,0)(InternalPairSemType))(0,0)(InternalPairSemType)
    val repr = lvalue.parse(exp).get
    assert(repr === nested_pair_elem)
  }
}



