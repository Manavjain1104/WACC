package src.main.scala.wacc


import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import wacc.AST._
import org.scalatest.funsuite.AnyFunSuite
import parsley.Parsley
import parsley.{Failure, Success}
import wacc.parser.expr

class Tests extends AnyFunSuite {

  test("Correct expression generated for Int-Liter expression") {
    val exp = "+2"
    val int_liter = IntExpr(2)
    val repr = expr.parse(exp).get
    assert(repr === int_liter)
  }

  test("Correct expression generated for negative Int-Liter expression") {
    val exp = "-2"
    val int_liter = NegExpr(IntExpr(2))
    val repr = expr.parse(exp).get
    assert(repr === int_liter)
  }

  test("Correct expression generated for Bool-Liter expression") {
    val exp = true && false
    val bool_liter = BoolExpr(exp)
    val repr = expr.parse(exp.toString).get
    assert(repr === bool_liter)
  }

  //TODO: how can we represent StringExpr? Any string "abcde" is an IdentExpr.
  /*
  test("Correct expression generated for Str-Liter expression") {
    val exp = "hello"
    val str_liter = StringExpr(exp)
    val repr = expr.parse(exp).get
    assert(repr === str_liter)
  }*/

  test("Correct expression generated for bracketed expressions") {
    val exp = "(((3+4)))"
    val brac = AddExpr(IntExpr(3),IntExpr(4))
    val repr = expr.parse(exp).get
    assert(repr === brac)
  }

  // TODO: (3+4)-2) doesn't error, we need to change this.
  /*
  test("Correct expression generated for nested expressions 1") {
    val exp = "((3+4)-2)"
    val nested = SubExpr(AddExpr(IntExpr(3), IntExpr(4)), IntExpr(2))
    val repr = expr.parse(exp).get
    assert(repr === nested)
  }*/

  test("Correct expression generated for nested expressions 2") {
    val exp = "(3*4)%(4/2)"
    val nested = ModExpr(MulExpr(IntExpr(3),IntExpr(4)),DivExpr(IntExpr(4),IntExpr(2)))
    val repr = expr.parse(exp).get
    assert(repr === nested)
  }


  test("Correct expression generated for Char-Liter expression") {
    val exp = 'b'
    val lt = CharExpr(exp)
    val repr = expr.parse("'b'").get
    assert(repr === lt)
  }

  test("Correct expression generated for Pair-literal expression") {
    val exp = "null"
    val pair_liter = PairExpr
    val repr = expr.parse(exp).get
    assert(repr === pair_liter)
  }

  test("Correct expression generated for add binop expression") {
    val exp = "2+3"
    val add = AddExpr(IntExpr(2), IntExpr(3))
    val repr = expr.parse(exp).get
    assert(repr === add)
  }

  test("Correct expression generated for subtract binop expression") {
    val exp = "3-2"
    val sub = SubExpr(IntExpr(3), IntExpr(2))
    val repr = expr.parse(exp).get
    assert(repr === sub)
  }

  test("Correct expression generated for multiply binop expression") {
    val exp = "2*3"
    val mul = MulExpr(IntExpr(2), IntExpr(3))
    val repr = expr.parse(exp).get
    assert(repr === mul)
  }

  test("Correct expression generated for divide binop expression") {
    val exp = "2/3"
    val div = DivExpr(IntExpr(2), IntExpr(3))
    val repr = expr.parse(exp).get
    assert(repr === div)
  }

  test("Correct expression generated for mod binop expression") {
    val exp = "2%3"
    val mod = ModExpr(IntExpr(2), IntExpr(3))
    val repr = expr.parse(exp).get
    assert(repr === mod)
  }

  test("Correct expression generated for gt binop expression") {
    val exp = "3>2"
    val gt = GTExpr(IntExpr(3), IntExpr(2))
    val repr = expr.parse(exp).get
    assert(repr === gt)
  }

  test("Correct expression generated for gteq binop expression") {
    val exp = "3>=2"
    val gteq = GTEQExpr(IntExpr(3), IntExpr(2))
    val repr = expr.parse(exp).get
    assert(repr === gteq)
  }

  test("Correct expression generated for lt binop expression") {
    val exp = "2<3"
    val lt = LTExpr(IntExpr(2), IntExpr(3))
    val repr = expr.parse(exp).get
    assert(repr === lt)
  }

  test("Correct expression generated for lteq binop expression") {
    val exp = "2<=3"
    val lteq = LTEQExpr(IntExpr(2), IntExpr(3))
    val repr = expr.parse(exp).get
    assert(repr === lteq)
  }

  test("Correct expression generated for equals binop expression") {
    val exp = "2==2"
    val eq = EQExpr(IntExpr(2), IntExpr(2))
    val repr = expr.parse(exp).get
    assert(repr === eq)
  }

  test("Correct expression generated for not equals binop expression") {
    val exp = "2!=3"
    val neq = NEQExpr(IntExpr(2), IntExpr(3))
    val repr = expr.parse(exp).get
    assert(repr === neq)
  }

  test("Correct expression generated for logical AND binop expression") {
    val exp = "a&&b"
    val log_and = AndExpr(IndentExpr("a"), IndentExpr("b"))
    val repr = expr.parse(exp).get
    assert(repr === log_and)
  }

  test("Correct expression generated for logical OR binop expression") {
    val exp = "a||b"
    val log_or = OrExpr(IndentExpr("a"), IndentExpr("b"))
    val repr = expr.parse(exp).get
    assert(repr === log_or)
  }

  test("Correct expression generated for not unop expression") {
    val exp = "!true"
    val not = NotExpr(BoolExpr(true))
    val repr = expr.parse(exp).get
    assert(repr === not)
  }

  test("Correct expression generated for neg unop expression") {
    val exp = "-2"
    val neg = NegExpr(IntExpr(2))
    val repr = expr.parse(exp).get
    assert(repr === neg)
  }

  test("Correct expression generated for len unop expression") {
    val exp = "len x"
    val len = LenExpr(IndentExpr("x"))
    val repr = expr.parse(exp).get
    assert(repr === len)
  }

  test("Correct expression generated for ord unop expression") {
    val exp = "ord a"
    val ord = OrdExpr(IndentExpr("a"))
    val repr = expr.parse(exp).get
    assert(repr === ord)
  }

  test("Correct expression generated for chr unop expression") {
    val exp = "chr 5"
    val chr = ChrExpr(IntExpr(5))
    val repr = expr.parse(exp).get
    assert(repr === chr)
  }
}

