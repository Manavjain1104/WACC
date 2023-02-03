package wacc

import parsley.Parsley
import parsley.genericbridges.{ParserBridge0, ParserBridge2, ParserSingletonBridge}
import parsley.lift.lift2
import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
import parsley.position.pos

object AST {

  // Parser Bridge Pattern Generics
  trait ParserBridgeSymPos1[-A, +B] {
    def apply(x: A)(symbolTable: Option[SymbolTable], pos: (Int, Int)): B
    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(x => (p : (Int, Int)) => this.apply(x)(None, p))
  }

  trait ParserSingletonPosBridge[+A] {
    def con(pos: (Int, Int)): A
    def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con) <* op
  }

  // Position Aware Parsers
  trait ParserBridgePos0[+A] extends ParserSingletonPosBridge[A] {
    def apply()(pos : (Int, Int)) : A
    override def con(pos: (Int, Int)): A = this.apply()(pos)
  }

  trait ParserBridgePos1[-A, +B] extends ParserSingletonPosBridge[A => B]{
    def apply(x: A)(pos: (Int, Int)): B
    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(x => (p : (Int, Int)) => this.apply(x)(p))
    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
  }

  trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonPosBridge[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C
    def apply(x: Parsley[A], y: =>Parsley[B]): Parsley[C] =
      pos <**> (x, y).zipped(this.apply(_, _) _)
    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
  }

  trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonPosBridge[(A, B, C) => D] {
    def apply(x: A, y: B, z:C)(pos: (Int, Int)): D
    def apply(x: Parsley[A], y: =>Parsley[B], z: =>Parsley[C]): Parsley[D] =
      pos <**> (x, y, z).zipped(this.apply(_, _, _) _)
    override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
  }

  trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonPosBridge[(A, B, C, D) => E] {
    def apply(x: A, y: B, z:C, w :D)(pos: (Int, Int)): E
    def apply(x: Parsley[A], y: =>Parsley[B], z: =>Parsley[C], w: =>Parsley[D]): Parsley[E] =
      pos <**> (x, y, z, w).zipped(this.apply(_, _, _, _) _)
    override final def con(pos: (Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
  }

  //  case class ScopeStat(stat: Statement)(var symtab: Option[SymbolTable]) extends Statement
  //
  //  object ScopeStat extends ParserBridge1[Statement, ScopeStat] {
  //    override def apply(stmt: Statement): ScopeStat = new ScopeStat(stmt)(None)
  //  }

  sealed trait AST

  // Higheest level program branch of AST
  case class Program(funcs: List[Func], stat: Statement)(val pos : (Int, Int)) extends AST

  object Program extends ParserBridgePos2[List[Func], Statement, Program]

  case class
    Func(retType: Type, ident: String, params: List[Param], stat: Statement)(val pos : (Int, Int))
    extends AST

  object Func extends ParserBridgePos4[Type, String, List[Param], Statement, Func]

  case class Param(paramType: Type, ident: String)(val pos : (Int, Int)) extends AST

  object Param extends ParserBridgePos2[Type, String, Param]

  // WACC Type heirarchy
  sealed trait Type extends AST

  sealed trait BaseType extends Type with PairElemType

  case class IntType()(val pos : (Int, Int)) extends BaseType
  object IntType extends ParserBridgePos0[BaseType]

  case class BoolType()(val pos : (Int, Int)) extends BaseType
  object BoolType extends ParserBridgePos0[BaseType]

  case class CharType()(val pos : (Int, Int)) extends BaseType
  object CharType extends ParserBridgePos0[BaseType]

  case class StringType()(val pos : (Int, Int)) extends BaseType
  object StringType extends ParserBridgePos0[BaseType]

  case class ArrayType(t: Type) extends Type with PairElemType
//  object ArrayType extends ParserBridge1[Type, Type]
  case class PairType(pt1: PairElemType, pt2: PairElemType)(val pos : (Int, Int)) extends Type
  object PairType extends ParserBridgePos2[PairElemType, PairElemType, Type]

  sealed trait PairElemType extends AST

  case object DummyPair extends PairElemType with ParserBridge0[PairElemType]

  // Expr heirarchy
  sealed trait Expr extends RValue

  case class IntExpr(x: Int)(val pos : (Int, Int)) extends Expr
  object IntExpr extends ParserBridgePos1[Int, Expr]

  case class BoolExpr(b: Boolean)(val pos : (Int, Int)) extends Expr
  object BoolExpr extends ParserBridgePos1[Boolean, Expr]

  case class CharExpr(c: Char)(val pos : (Int, Int)) extends Expr

  object CharExpr extends ParserBridgePos1[Char, Expr]

  case class StringExpr(s: String)(val pos : (Int, Int)) extends Expr

  object StringExpr extends ParserBridgePos1[String, Expr]

  case object PairExpr extends Expr with ParserBridge0[Expr]

  case class IdentExpr(ident: String)(val pos : (Int, Int)) extends Expr
  object IdentExpr extends ParserBridgePos1[String, Expr]

  sealed trait UnopExpr extends Expr

  case class NotExpr(e: Expr)(val pos : (Int, Int)) extends UnopExpr
  object NotExpr extends ParserBridgePos1[Expr, UnopExpr]

  case class NegExpr(e: Expr)(val pos : (Int, Int)) extends UnopExpr
  object NegExpr extends ParserBridgePos1[Expr, UnopExpr]

  case class LenExpr(e: Expr)(val pos : (Int, Int)) extends UnopExpr
  object LenExpr extends ParserBridgePos1[Expr, UnopExpr]

  case class OrdExpr(e: Expr)(val pos : (Int, Int)) extends UnopExpr
  object OrdExpr extends ParserBridgePos1[Expr, UnopExpr]

  case class ChrExpr(e: Expr)(val pos : (Int, Int)) extends UnopExpr
  object ChrExpr extends ParserBridgePos1[Expr, UnopExpr]

  sealed trait BinopExpr extends Expr

  case class MulExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object MulExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class DivExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object DivExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class ModExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object ModExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class AddExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object AddExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class SubExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object SubExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class GTExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object GTExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class GTEQExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object GTEQExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class LTExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object LTExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class LTEQExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object LTEQExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class EQExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object EQExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class NEQExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object NEQExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class AndExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object AndExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]

  case class OrExpr(e1: Expr, e2: Expr)(val pos : (Int, Int)) extends BinopExpr
  object OrExpr extends ParserBridgePos2[Expr, Expr, BinopExpr]


  sealed trait PairElem extends LValue with RValue

  case class Fst(lvalue: LValue)(val pos : (Int, Int)) extends PairElem

  object Fst extends ParserBridgePos1[LValue, PairElem]

  case class Snd(lvalue: LValue)(val pos : (Int, Int)) extends PairElem

  object Snd extends ParserBridgePos1[LValue, PairElem]

  // Statement branch of AST
  sealed trait Statement extends AST

  case object Skip extends Statement with ParserBridge0[Statement]

  case class VarDec(assignType: Type, ident: String, rvalue: RValue)(val pos: (Int, Int)) extends Statement

  object VarDec extends ParserBridgePos3[Type, String, RValue, Statement]

  case class Assign(lvalue: LValue, rvalue: RValue)(val pos : (Int, Int)) extends Statement

  object Assign extends ParserBridgePos2[LValue, RValue, Statement]

  case class Read(lvalue: LValue)(val pos : (Int, Int)) extends Statement

  object Read extends ParserBridgePos1[LValue, Statement]

  case class Free(e: Expr)(val pos : (Int, Int)) extends Statement

  object Free extends ParserBridgePos1[Expr, Statement]

  case class Return(e: Expr)(val pos : (Int, Int)) extends Statement

  object Return extends ParserBridgePos1[Expr, Statement]

  case class Exit(e: Expr)(val pos : (Int, Int)) extends Statement

  object Exit extends ParserBridgePos1[Expr, Statement]

  case class Print(e: Expr)(val pos : (Int, Int)) extends Statement

  object Print extends ParserBridgePos1[Expr, Statement]

  case class Println(e: Expr)(val pos : (Int, Int)) extends Statement

  object Println extends ParserBridgePos1[Expr, Statement]

  case class If(cond: Expr, thenStat: Statement, elseStat: Statement)(val pos : (Int, Int)) extends Statement

  object If extends ParserBridgePos3[Expr, Statement, Statement, Statement]

  case class While(cond: Expr, doStat: Statement)(val pos : (Int, Int)) extends Statement

  object While extends ParserBridgePos2[Expr, Statement, Statement]

//  case class ScopeStat(stat: Statement)(var symtab: Option[SymbolTable], val pos : (Int, Int)) extends Statement

  case class ScopeStat(stat: Statement)(val pos : (Int, Int)) extends Statement

  object ScopeStat extends ParserBridgePos1[Statement, ScopeStat]


  case class ConsecStat(first: Statement, next: Statement) extends Statement

  // LValue branch of AST
  sealed trait LValue extends AST

  case class IdentValue(s: String)(val pos : (Int, Int)) extends LValue

  object IdentValue extends ParserBridgePos1[String, LValue]

  case class ArrayElem(ident: String, exprs: List[Expr])(val pos : (Int, Int)) extends LValue with Expr

  object ArrayElem extends ParserBridgePos2[String, List[Expr], ArrayElem]

  // RValue branch of AST
  sealed trait RValue extends AST

  case class ArrayLiter(exprs: List[Expr])(val pos : (Int, Int)) extends RValue

  object ArrayLiter extends ParserBridgePos1[List[Expr], RValue]

  case class NewPair(expr1: Expr, expr2: Expr)(val pos : (Int, Int)) extends RValue

  object NewPair extends ParserBridgePos2[Expr, Expr, RValue]

  case class Call(ident: String, args: List[Expr])(val pos : (Int, Int)) extends RValue

  object Call extends ParserBridgePos2[String, List[Expr], RValue]
}
