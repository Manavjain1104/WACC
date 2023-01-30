package wacc

import parsley.genericbridges.{ParserBridge0, ParserBridge1}

object AST {

  // Higheest level program branch of AST
  case class Program(funcs: List[FuncAST], stat: Statement)
  case class FuncAST(retType: Type, ident: String, params: List[Param], stat: Statement)
  case class Param (paramType : Type, ident : String)

  // WACC Type heirarchy
  sealed trait Type

  sealed trait BaseType extends Type with PairElemType
  case object IntType extends BaseType
  case object BoolType extends BaseType
  case object CharType extends BaseType
  case object StringType extends BaseType
  case class ArrayType(t: Type) extends Type with PairElemType
  case class PairType(pt1: PairElemType, pt2: PairElemType) extends Type
  sealed trait PairElemType
  case object EmptyPair extends PairElemType


  // Expr heirarchy
  sealed trait Expr extends RValue

  case class IntExpr(x: BigInt) extends Expr
  object IntExpr extends ParserBridge1[BigInt, Expr]
//  IntExpr(Int) -- > Parsley[Expr]
  case class BoolExpr(b: Boolean) extends Expr
  object BoolExpr extends ParserBridge1[Boolean, Expr]
  case class CharExpr(c: Char) extends Expr
  object CharExpr extends ParserBridge1[Char, Expr]
  case class StringExpr(s: String) extends Expr
  object StringExpr extends ParserBridge1[String, Expr]
  case object PairExpr extends Expr with ParserBridge0[Expr]

  case class IndentExpr(ident: String) extends Expr
  object IndentExpr extends ParserBridge1[String, Expr]


  sealed trait UnopExpr extends Expr
  case class NotExpr(e : Expr) extends UnopExpr
  case class NegExpr(e : Expr) extends UnopExpr
  case class LenExpr(e : Expr) extends UnopExpr
  case class OrdExpr(e : Expr) extends UnopExpr
  case class ChrExpr(e : Expr) extends UnopExpr

  sealed trait BinopExpr extends Expr
  case class MulExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class DivExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class ModExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class AddExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class SubExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class GTExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class GTEQExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class LTExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class LTEQExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class EQExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class NEQExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class AndExpr(e1 : Expr, e2 : Expr) extends BinopExpr
  case class OrExpr(e1 : Expr, e2 : Expr) extends BinopExpr


  sealed trait PairElem extends LValue with RValue
  case class Fst(lvalue: LValue) extends PairElem
  case class Snd(lvalue: LValue) extends PairElem


  // Statement branch of AST
  sealed trait Statement
  case object Skip extends Statement
  case class AssignEq(assignType: Type, ident: String, rvalue: RValue) extends Statement
  case class Equals(lvalue: LValue, rvalue: RValue)
  case class Read(lvalue: LValue) extends Statement
  case class Free(e: Expr) extends Statement
  case class Return(e: Expr) extends Statement
  case class Exit(e: Expr) extends Statement
  case class Print(e: Expr) extends Statement
  case class Println(e: Expr) extends Statement
  case class If(cond: Expr, thenStat: Statement, elseStat: Statement) extends Statement
  case class While(cond: Expr, doStat: Statement) extends Statement
  case class ScopeStat(stat: Statement) extends Statement
  case class ConsecStat(first: Statement, next: Statement) extends Statement


  // LValue branch of AST
  sealed trait LValue
  case class IdentValue(s: String) extends LValue

  // RValue branch of AST
  sealed trait RValue
  case class ArrayLiter(exprs: List[Expr]) extends RValue
  case class NewPair(expr1: Expr, expr2: Expr) extends RValue
  case class Call(ident: String, args: List[Expr]) extends RValue

}
