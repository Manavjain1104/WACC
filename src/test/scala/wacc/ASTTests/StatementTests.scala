package wacc.ASTTests

import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import wacc.AST._
import wacc.parser._
object StatementTests extends Tag("StatementTests")

class StatementTests extends AnyFlatSpec {

  behavior of "skip statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "skip"
    val skip_stat = Skip
    val repr = statement.parse(exp).get
    assert(repr === skip_stat)
  }

  behavior of "variable assignment statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "int x = 5"
    val var_ass = VarDec(IntType()(0,0),"x",IntExpr(5)(0,0))(None, (0,0))
    val repr = statement.parse(exp).get
    assert(repr === var_ass)
  }

  behavior of "eq statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "y = 5"
    val eq = Assign(IdentValue("y")(None, (0,0)),IntExpr(5)(0,0))(0,0)
    val repr = statement.parse(exp).get
    assert(repr === eq)
  }

  behavior of "read statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "read abc"
    val read_stmt = Read(IdentValue("abc")(None, (0,0)))(None,(0,0))
    val repr = statement.parse(exp).get
    assert(repr === read_stmt)
  }

  behavior of "return statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "return 5"
    val ret_stmt = Return(IntExpr(5)(0,0))(0,0)
    val repr = statement.parse(exp).get
    assert(repr === ret_stmt)
  }

  behavior of "exit statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "exit 0"
    val exit_stmt = Exit(IntExpr(0)(0,0))(0,0)
    val repr = statement.parse(exp).get
    assert(repr === exit_stmt)
  }

  behavior of "free statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "free 5"
    val free_stmt = Free(IntExpr(5)(0,0))(None,(0,0))
    val repr = statement.parse(exp).get
    assert(repr === free_stmt)
  }

  behavior of "print statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "print 5"
    val print_stmt = Print(IntExpr(5)(0,0))(None,(0,0))
    val repr = statement.parse(exp).get
    assert(repr === print_stmt)
  }

  behavior of "println statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "println 5"
    val println_stmt = Println(IntExpr(5)(0,0))(None,(0,0))
    val repr = statement.parse(exp).get
    assert(repr === println_stmt)
  }

  behavior of "if statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "if x then return y else skip fi"
    val if_stmt = If(IdentExpr("x")(None,(0,0)),Return(IdentExpr("y")(None,(0,0)))(0,0),Skip)(0,0)
    val repr = statement.parse(exp).get
    assert(repr === if_stmt)
  }

  behavior of "while statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp =  "while x do if y then skip else return z fi done"
    val while_stmt = While(IdentExpr("x")(None,(0,0)),If(IdentExpr("y")(None,(0,0)),Skip,Return(IdentExpr("z")(None,(0,0)))(0,0))(0,0))(0,0)
    val repr = statement.parse(exp).get
    assert(repr === while_stmt)
  }

  behavior of "scope statement"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "begin while x do if y then skip else return z fi done end"
    val scope_stmt = ScopeStat(While(IdentExpr("x")(None,(0,0)),If(IdentExpr("y")(None, (0,0)),Skip,
      Return(IdentExpr("z")(None, (0,0)))(0,0))(0,0))(0,0))(0,0)
    val repr = statement.parse(exp).get
    assert(repr === scope_stmt)
  }

  behavior of "consecutive statements"
  it should "generate the correct statement" taggedAs(StatementTests, SyntacticTests) in {
    val exp = "read abc; return 5; exit 100"
    val consecutive_stmts = ConsecStat(ConsecStat(Read(IdentValue("abc")(None, (0,0)))(None,(0,0)),
      Return(IntExpr(5)(0,0))(0,0)),Exit(IntExpr(100)(0,0))(0,0))
    val repr = statement.parse(exp).get
    assert(repr === consecutive_stmts)
  }

}
