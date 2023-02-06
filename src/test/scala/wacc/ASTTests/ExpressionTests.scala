package src.main.scala.wacc


import org.scalatest.Tag
import wacc.AST._
import org.scalatest.funsuite.AnyFunSuite
import wacc.ASTTests.SyntacticTests
import wacc.parser.expr

object ExpressionTests extends Tag("ExpressionTests")
class ExpressionTests extends AnyFunSuite with SyntacticTests {

  test("Correct expression generated for Int-Liter expression", ExpressionTests, SyntacticTests) {
    val exp = "+2"
    val int_liter = IntExpr(2)(0, 0)
    val repr = expr.parse(exp).get
    assert(repr === int_liter)
  }

  test("Correct expression generated for negative Int-Liter expression",
    ExpressionTests, SyntacticTests) {
    val exp = "-2"
    val int_liter = IntExpr(-2)(0,0)
    val repr = expr.parse(exp).get
    assert(repr === int_liter)
  }

  test("Correct expression generated for Bool-Liter expression", ExpressionTests, SyntacticTests) {
    val exp = true && false
    val bool_liter = BoolExpr(exp)(0,0)
    val repr = expr.parse(exp.toString).get
    assert(repr === bool_liter)
  }

  //TODO: how can we represent StringExpr? Any string "abcde" is an IdentExpr.

//  test("Correct expression generated for Str-Liter expression", ExpressionTests) {
//    val exp = "string x = hello"
//    val str_liter = StringExpr("hello")(0,0)
//    val stmt : Assign = Assign()
//    val repr = expr.parse(exp).get
//    assert(repr === str_liter)
//  }

  test("Correct expression generated for bracketed expressions", ExpressionTests, SyntacticTests) {
    val exp = "(((3+4)))"
    val brac = AddExpr(IntExpr(3)(0,0),IntExpr(4)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === brac)
  }


  test("Correct expression generated for nested expressions 1", ExpressionTests, SyntacticTests) {
    val exp = "((3+4)-2)"
    val nested = SubExpr(AddExpr(IntExpr(3)(0,0), IntExpr(4)(0,0))(0,0), IntExpr(2)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === nested)
  }

  test("Correct expression generated for nested expressions 2", ExpressionTests, SyntacticTests) {
    val exp = "(3*4)%(4/2)"
    val nested = ModExpr(MulExpr(IntExpr(3)(0,0),IntExpr(4)(0,0))(0,0),DivExpr(IntExpr(4)(0,0),IntExpr(2)(0,0))(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === nested)
  }


  test("Correct expression generated for Char-Liter expression", ExpressionTests, SyntacticTests) {
    val exp = 'b'
    val lt = CharExpr(exp)(0,0)
    val repr = expr.parse("'b'").get
    assert(repr === lt)
  }

  test("Correct expression generated for Pair-literal expression", ExpressionTests, SyntacticTests) {
    val exp = "null"
    val pair_liter = PairExpr
    val repr = expr.parse(exp).get
    assert(repr === pair_liter)
  }

  test("Correct expression generated for add binop expression", ExpressionTests, SyntacticTests) {
    val exp = "2+3"
    val add = AddExpr(IntExpr(2)(0,0), IntExpr(3)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === add)
  }

  test("Correct expression generated for subtract binop expression", ExpressionTests, SyntacticTests) {
    val exp = "3-2"
    val sub = SubExpr(IntExpr(3)(0,0), IntExpr(2)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === sub)
  }

  test("Correct expression generated for multiply binop expression", ExpressionTests, SyntacticTests) {
    val exp = "2*3"
    val mul = MulExpr(IntExpr(2)(0,0), IntExpr(3)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === mul)
  }

  test("Correct expression generated for divide binop expression", ExpressionTests, SyntacticTests) {
    val exp = "2/3"
    val div = DivExpr(IntExpr(2)(0,0), IntExpr(3)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === div)
  }

  test("Correct expression generated for mod binop expression", ExpressionTests, SyntacticTests) {
    val exp = "2%3"
    val mod = ModExpr(IntExpr(2)(0,0), IntExpr(3)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === mod)
  }

  test("Correct expression generated for gt binop expression", ExpressionTests, SyntacticTests) {
    val exp = "3>2"
    val gt = GTExpr(IntExpr(3)(0,0), IntExpr(2)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === gt)
  }

  test("Correct expression generated for gteq binop expression", ExpressionTests, SyntacticTests) {
    val exp = "3>=2"
    val gteq = GTEQExpr(IntExpr(3)(0,0), IntExpr(2)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === gteq)
  }

  test("Correct expression generated for lt binop expression", ExpressionTests, SyntacticTests) {
    val exp = "2<3"
    val lt = LTExpr(IntExpr(2)(0,0), IntExpr(3)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === lt)
  }

  test("Correct expression generated for lteq binop expression", ExpressionTests, SyntacticTests) {
    val exp = "2<=3"
    val lteq = LTEQExpr(IntExpr(2)(0,0), IntExpr(3)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === lteq)
  }

  test("Correct expression generated for equals binop expression", ExpressionTests, SyntacticTests) {
    val exp = "2==2"
    val eq = EQExpr(IntExpr(2)(0,0), IntExpr(2)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === eq)
  }

  test("Correct expression generated for not equals binop expression", ExpressionTests, SyntacticTests) {
    val exp = "2!=3"
    val neq = NEQExpr(IntExpr(2)(0,0), IntExpr(3)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === neq)
  }

  test("Correct expression generated for logical AND binop expression", ExpressionTests, SyntacticTests) {
    val exp = "a&&b"
    val log_and = AndExpr(IdentExpr("a")(0,0), IdentExpr("b")(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === log_and)
  }

  test("Correct expression generated for logical OR binop expression", ExpressionTests, SyntacticTests) {
    val exp = "a||b"
    val log_or = OrExpr(IdentExpr("a")(0,0), IdentExpr("b")(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === log_or)
  }

  test("Correct expression generated for not unop expression", ExpressionTests, SyntacticTests) {
    val exp = "!true"
    val not = NotExpr(BoolExpr(true)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === not)
  }

  test("Correct expression generated for neg unop expression", ExpressionTests, SyntacticTests) {
    val exp = "-2"
    val neg = (IntExpr(-2)(0,0))
    val repr = expr.parse(exp).get
    assert(repr === neg)
  }

  test("Correct expression generated for len unop expression", ExpressionTests, SyntacticTests) {
    val exp = "len x"
    val len = LenExpr(IdentExpr("x")(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === len)
  }

  test("Correct expression generated for ord unop expression", ExpressionTests, SyntacticTests) {
    val exp = "ord a"
    val ord = OrdExpr(IdentExpr("a")(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === ord)
  }

  test("Correct expression generated for chr unop expression", ExpressionTests, SyntacticTests) {
    val exp = "chr 5"
    val chr = ChrExpr(IntExpr(5)(0,0))(0,0)
    val repr = expr.parse(exp).get
    assert(repr === chr)
  }
}

