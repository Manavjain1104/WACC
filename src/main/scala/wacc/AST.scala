package wacc

import parsley.genericbridges.{ParserBridge0, ParserBridge1, ParserBridge2, ParserBridge3}

object AST {

  // Higheest level program branch of AST
  case class Program(funcs: List[FuncAST], stat: Statement)
  case class FuncAST(retType: Type, ident: String, params: List[Param], stat: Statement)
  case class Param (paramType : Type, ident : String)

  // WACC Type heirarchy
  sealed trait Type
  sealed trait BaseType extends Type with PairElemType
  case object IntType extends BaseType with ParserBridge0[BaseType]
  case object BoolType extends BaseType with ParserBridge0[BaseType]
  case object CharType extends BaseType with ParserBridge0[BaseType]
  case object StringType extends BaseType with ParserBridge0[BaseType]
  case class ArrayType(t: Type) extends Type with PairElemType
//  object ArrayType extends ParserBridge1[Type, Type]
  case class PairType(pt1: PairElemType, pt2: PairElemType) extends Type
  object PairType extends ParserBridge2[PairElemType, PairElemType, Type]
  sealed trait PairElemType
  case object DummyPair extends PairElemType with ParserBridge0[PairElemType]

  // Expr heirarchy
  sealed trait Expr extends RValue

  case class IntExpr(x: BigInt) extends Expr
  object IntExpr extends ParserBridge1[BigInt, Expr]
  case class BoolExpr(b: Boolean) extends Expr
  object BoolExpr extends ParserBridge1[Boolean, Expr]
  case class CharExpr(c: Char) extends Expr
  object CharExpr extends ParserBridge1[Char, Expr]
  case class StringExpr(s: String) extends Expr
  object StringExpr extends ParserBridge1[String, Expr]
  case object PairExpr extends Expr with ParserBridge0[Expr]

  case class IdentExpr(ident: String) extends Expr
  object IdentExpr extends ParserBridge1[String, Expr]


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
  object Fst extends ParserBridge1[LValue, PairElem]
  case class Snd(lvalue: LValue) extends PairElem
  object Snd extends ParserBridge1[LValue, PairElem]

  // Statement branch of AST
  sealed trait Statement
  case object Skip extends Statement with ParserBridge0[Statement]
  case class AssignEq(assignType: Type, ident: String, rvalue: RValue) extends Statement
  object AssignEq extends ParserBridge3[Type, String, RValue, Statement]
  case class Equals(lvalue: LValue, rvalue: RValue) extends Statement
  object Equals extends ParserBridge2[LValue, RValue, Statement]
  case class Read(lvalue: LValue) extends Statement
  object Read extends ParserBridge1[LValue, Statement]
  case class Free(e: Expr) extends Statement
  object Free extends ParserBridge1[Expr, Statement]
  case class Return(e: Expr) extends Statement
  object Return extends ParserBridge1[Expr, Statement]
  case class Exit(e: Expr) extends Statement
  object Exit extends ParserBridge1[Expr, Statement]
  case class Print(e: Expr) extends Statement
  object Print extends ParserBridge1[Expr, Statement]
  case class Println(e: Expr) extends Statement
  object Println extends ParserBridge1[Expr, Statement]
  case class If(cond: Expr, thenStat: Statement, elseStat: Statement) extends Statement
  object If extends ParserBridge3[Expr, Statement, Statement, Statement]
  case class While(cond: Expr, doStat: Statement) extends Statement
  object While extends ParserBridge2[Expr, Statement, Statement]
  case class ScopeStat(stat: Statement) extends Statement
  object ScopeStat extends ParserBridge1[Statement, Statement]
  case class ConsecStat(first: Statement, next: Statement) extends Statement
//  object ConsecStat extends ParserBridge2[Statement, Statement, Statement]


  // LValue branch of AST
  sealed trait LValue
  case class IdentValue(s: String) extends LValue
  object IdentValue extends ParserBridge1[String, LValue]
  case class ArrayElem(ident : String, exprs : List[Expr]) extends LValue with Expr
  object ArrayElem extends ParserBridge2[String, List[Expr], ArrayElem]

  // RValue branch of AST
  sealed trait RValue
  case class ArrayLiter(exprs: List[Expr]) extends RValue
  object ArrayLiter extends ParserBridge1[List[Expr], RValue]
  case class NewPair(expr1: Expr, expr2: Expr) extends RValue
  object NewPair extends ParserBridge2[Expr, Expr, RValue]
  case class Call(ident: String, args: List[Expr]) extends RValue
  object Call extends ParserBridge2[String, List[Expr], RValue]
}
