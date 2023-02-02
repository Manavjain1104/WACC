package wacc.ASTTests

import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import wacc.AST.{ArrayElem, Fst, IdentValue, IntExpr, Snd}
import wacc.parser._

object LValueTests extends Tag("LValueTests")

class LValueTests extends AnyFlatSpec {

  behavior of "ident LValue"
  it should "generate the correct ident LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "abcde"
    val lvalue_ident = IdentValue(exp)
    val repr = lvalue.parse(exp).get
    assert(repr === lvalue_ident)
  }

  behavior of "array-elem LValue"
  it should "generate the correct single array-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "arr[3]"
    val array_elem_1 = ArrayElem("arr",List(IntExpr(3)))
    val repr = lvalue.parse(exp).get
    assert(repr === array_elem_1)
  }

  behavior of "array-elem LValue"
  it should "generate the correct multiple array-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "arr[3][4][5]"
    val array_elem_2 = ArrayElem("arr", List(IntExpr(3), IntExpr(4), IntExpr(5)))
    val repr = lvalue.parse(exp).get
    assert(repr === array_elem_2)
  }

  behavior of "ident pair-elem LValue"
  it should "generate the correct ident pair-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "fst abcde"
    val pair_elem_ident = Fst(IdentValue("abcde"))
    val repr = lvalue.parse(exp).get
    assert(repr === pair_elem_ident)
  }

  behavior of "array-elem pair-elem LValue"
  it should "generate the correct array-elem pair-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "snd arr[1][2]"
    val pair_elem_array_elem = Snd(ArrayElem("arr",List(IntExpr(1), IntExpr(2))))
    val repr = lvalue.parse(exp).get
    assert(repr === pair_elem_array_elem)
  }

  behavior of "nested pair-elem LValue"
  it should "generate the correct nested pair-elem LValue expression" taggedAs(LValueTests, SyntacticTests) in {
    val exp = "fst snd abcde"
    val nested_pair_elem = Fst(Snd(IdentValue("abcde")))
    val repr = lvalue.parse(exp).get
    assert(repr === nested_pair_elem)
  }
}


