package src.main.scala.wacc

import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import wacc.AST._
import wacc.ASTTests.SyntacticTests
import wacc.parser.{expr, rvalue}

object RValueTests extends Tag("RValueTests")

class RValueTests extends AnyFlatSpec {

  behavior of "ident RValue"
  it should "generate the correct RValue expression" taggedAs(RValueTests, SyntacticTests) in {
    val exp = "abcde"
    val rvalue_ident = IdentExpr(exp)(None,(0,0))
    val repr = rvalue.parse(exp).get
    assert(repr === rvalue_ident)
  }

  behavior of "arraylit RValue"
  it should "generate the correct RValue arraylist" taggedAs(RValueTests, SyntacticTests) in {
    val exp = "[1,2,3]"
    val array_lit = ArrayLiter(List(IntExpr(1)(0,0),IntExpr(2)(0,0),IntExpr(3)(0,0)))(0,0)
    val repr = rvalue.parse(exp).get
    assert(repr === array_lit)
  }

  behavior of "NewPair RValue"
  it should "generate the correct RValue NewPair" taggedAs(RValueTests, SyntacticTests) in {
    val exp = "newpair(3,2)"
    val newp = NewPair(IntExpr(3)(0,0),IntExpr(2)(0,0))(0,0)
    val repr = rvalue.parse(exp).get
    assert(repr === newp)
  }

  behavior of "ident pair-elem RValue"
  it should "generate the correct ident pair-elem RValue expression" taggedAs(RValueTests, SyntacticTests) in {
    val exp = "fst abcde"
    val pair_elem_ident = Fst(IdentValue("abcde")(0,0))(0,0)
    val repr = rvalue.parse(exp).get
    assert(repr === pair_elem_ident)
  }

  behavior of "array-elem pair-elem RValue"
  it should "generate the correct array-elem pair-elem RValue expression" taggedAs(RValueTests, SyntacticTests) in {
    val exp = "snd arr[1][2]"
    val pair_elem_array_elem = Snd(ArrayElem("arr", List(IntExpr(1)(0,0), IntExpr(2)(0,0)))(None,(0,0)))(0,0)
    val repr = rvalue.parse(exp).get
    assert(repr === pair_elem_array_elem)
  }

  behavior of "nested pair-elem RValue"
  it should "generate the correct nested pair-elem RValue expression" taggedAs(RValueTests, SyntacticTests) in {
    val exp = "fst snd abcde"
    val nested_pair_elem = Fst(Snd(IdentValue("abcde")(0,0))(0,0))(0,0)
    val repr = rvalue.parse(exp).get
    assert(repr === nested_pair_elem)
  }

  behavior of "Call RValue"
  it should "generate the correct RValue Call" taggedAs(RValueTests, SyntacticTests) in {
    val exp = "call arr(3,2)"
    val cll = Call("arr",List(IntExpr(3)(0,0), IntExpr(2)(0,0)))(0,0)
    val repr = rvalue.parse(exp).get
    assert(repr === cll)
  }
}