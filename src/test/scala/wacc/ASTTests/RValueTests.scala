package src.main.scala.wacc

import org.scalatest.flatspec.AnyFlatSpec
import wacc.AST._
import wacc.parser.{expr, rvalue}

class RValueTests extends  AnyFlatSpec {

  behavior of "ident RValue"
  it should "generate the correct RValue expression" in {
    val exp = "abcde"
    val rvalue_ident = IdentExpr(exp)
    val repr = rvalue.parse(exp).get
    assert(repr === rvalue_ident)
  }

  behavior of "arraylit RValue"
  it should "generate the correct RValue arraylist" in {
    val exp = "[1,2,3]"
    val array_lit = ArrayLiter(List(IntExpr(1),IntExpr(2),IntExpr(3)))
    val repr = rvalue.parse(exp).get
    assert(repr === array_lit)
  }

  behavior of "NewPair RValue"
  it should "generate the correct RValue NewPair" in {
    val exp = "newpair(3,2)"
    val newp = NewPair(IntExpr(3),IntExpr(2))
    val repr = rvalue.parse(exp).get
    assert(repr === newp)
  }

  behavior of "ident pair-elem RValue"
  it should "generate the correct ident pair-elem RValue expression" in {
    val exp = "fst abcde"
    val pair_elem_ident = Fst(IdentValue("abcde"))
    val repr = rvalue.parse(exp).get
    assert(repr === pair_elem_ident)
  }

  behavior of "array-elem pair-elem RValue"
  it should "generate the correct array-elem pair-elem RValue expression" in {
    val exp = "snd arr[1][2]"
    val pair_elem_array_elem = Snd(ArrayElem("arr", List(IntExpr(1), IntExpr(2))))
    val repr = rvalue.parse(exp).get
    assert(repr === pair_elem_array_elem)
  }

  behavior of "nested pair-elem RValue"
  it should "generate the correct nested pair-elem RValue expression" in {
    val exp = "fst snd abcde"
    val nested_pair_elem = Fst(Snd(IdentValue("abcde")))
    val repr = rvalue.parse(exp).get
    assert(repr === nested_pair_elem)
  }

  behavior of "Call RValue"
  it should "generate the correct RValue Call" in {
    val exp = "call arr(3,2)"
    val cll = Call("arr",List(IntExpr(3), IntExpr(2)))
    val repr = rvalue.parse(exp).get
    assert(repr === cll)
  }
}