package wacc

import parsley.Parsley
import parsley.genericbridges.ParserBridge0
import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
import parsley.position.pos
import wacc.SemTypes.InternalPairSemType

object AST {

  // * Parser Bridge Pattern Generics * //

  // * Symbol Table and Position Aware Bridges * //

  trait ParserBridgeSymPos1[-A, +B] extends ParserSingletonPosBridge[A => B]{
    def apply(x: A)(symbolTable: Option[SymbolTable[SemTypes.SemType]], pos: (Int, Int)): B

    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(x => (p: (Int, Int)) => this.apply(x)(None, p))

    override final def con(pos: (Int, Int)): A => B = this.apply(_)(None, pos)
  }

  trait ParserBridgeSymPos2[-A, -B, +C] extends ParserSingletonPosBridge[(A,B) => C]{
    def apply(x: A, y: B)(st: Option[SymbolTable[SemTypes.SemType]], pos: (Int, Int)): C

    def apply(x: Parsley[A], y: Parsley[B]): Parsley[C]
    = pos <**> (x, y).zipped((a: A, b: B) => (p: (Int, Int)) => this.apply(a, b)(None, p))

    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(None, pos)
  }

  trait ParserBridgeSymPos3[-A, -B, -C, +D] {
    def apply(x: A, y: B, z: C)(st: Option[SymbolTable[SemTypes.SemType]], pos: (Int, Int)): D

    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C]): Parsley[D]
    = pos <**> (x, y, z).zipped((a: A, b: B, c: C) => (p: (Int, Int)) => this.apply(a, b, c)(None, p))
  }

  trait ParserBridgeSymPos4[-A, -B, -C, -D, +E] {
    def apply(x: A, y: B, z: C, w: D)(st: Option[SymbolTable[SemTypes.SemType]], pos: (Int, Int)): E

    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C], w: Parsley[D]): Parsley[E]
    = pos <**> (x, y, z, w).zipped((a: A, b: B, c: C, d: D) => (p: (Int, Int)) => this.apply(a, b, c, d)(None, p))
  }

  // * Only Position Aware Parsers * //

  trait ParserSingletonPosBridge[+A] {
    def con(pos: (Int, Int)): A

    def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con) <* op
  }

  trait ParserBridgePos0[+A] extends ParserSingletonPosBridge[A] {
    def apply()(pos: (Int, Int)): A

    override def con(pos: (Int, Int)): A = this.apply()(pos)
  }

  trait ParserBridgePos1[-A, +B] extends ParserSingletonPosBridge[A => B] {
    def apply(x: A)(pos: (Int, Int)): B

    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(x => (p: (Int, Int)) => this.apply(x)(p))

    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
  }

  trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonPosBridge[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C

    def apply(x: Parsley[A], y: => Parsley[B]): Parsley[C] =
      pos <**> (x, y).zipped((a: A, b: B) => this.apply(a, b))

    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
  }

  trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonPosBridge[(A, B, C) => D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D

    def apply(x: Parsley[A], y: => Parsley[B], z: => Parsley[C]): Parsley[D] =
      pos <**> (x, y, z).zipped((a: A, b: B, c: C) => this.apply(a, b, c))

    override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
  }

  trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonPosBridge[(A, B, C, D) => E] {
    def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E

    def apply(x: Parsley[A], y: => Parsley[B], z: => Parsley[C], w: => Parsley[D]): Parsley[E] =
      pos <**> (x, y, z, w).zipped((a: A, b: B, c: C, d: D) => this.apply(a, b, c, d))

    override final def con(pos: (Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
  }

  // * Abstract Syntax Tree * //
  sealed trait AST

  // * Highest Level Program Branch of the Abstract Syntax Tree * //
  case class Program(funcs: List[Func], stat: Statement)(val pos: (Int, Int)) extends AST

  object Program extends ParserBridgePos2[List[Func], Statement, Program]

  case class
  Func(retType: Type, ident: String, params: List[Param], stat: Statement)(var st: Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int))
    extends AST

  object Func extends ParserBridgeSymPos3[(Type, String) , List[Param], Statement, Func] {
    override def apply(x: (Type, String), y: List[Param], z: Statement)(st: Option[SymbolTable[SemTypes.SemType]], pos: (Int, Int)): Func = {
      Func(x._1, x._2, y, z)(st, pos)
    }
  }

  case class Param(paramType: Type, ident: String)(val pos: (Int, Int)) extends AST

  object Param extends ParserBridgePos2[Type, String, Param]

  // WACC Type hierarchy

  sealed trait Type extends AST

  sealed trait BaseType extends Type with PairElemType

  case class IntType()(val pos: (Int, Int)) extends BaseType

  object IntType extends ParserBridgePos0[BaseType]

  case class BoolType()(val pos: (Int, Int)) extends BaseType

  object BoolType extends ParserBridgePos0[BaseType]

  case class CharType()(val pos: (Int, Int)) extends BaseType

  object CharType extends ParserBridgePos0[BaseType]

  case class StringType()(val pos: (Int, Int)) extends BaseType

  object StringType extends ParserBridgePos0[BaseType]

  case class ArrayType(t: Type) extends Type with PairElemType

  case class PairType(pt1: PairElemType, pt2: PairElemType)(val pos: (Int, Int)) extends Type

  object PairType extends ParserBridgePos2[PairElemType, PairElemType, Type]

  sealed trait PairElemType extends AST

  case object DummyPair extends PairElemType with ParserBridge0[PairElemType]

  // Expr hierarchy
  sealed trait Expr extends RValue

  case class IntExpr(x: Int)(val pos: (Int, Int)) extends Expr

  object IntExpr extends ParserBridgePos1[Int, Expr]

  case class BoolExpr(b: Boolean)(val pos: (Int, Int)) extends Expr

  object BoolExpr extends ParserBridgePos1[Boolean, Expr]

  case class CharExpr(c: Char)(val pos: (Int, Int)) extends Expr

  object CharExpr extends ParserBridgePos1[Char, Expr]

  case class StringExpr(s: String)(val pos: (Int, Int)) extends Expr

  object StringExpr extends ParserBridgePos1[String, Expr]

  case class PairExpr()(val pos: (Int, Int)) extends Expr

  object PairExpr extends ParserBridgePos0[Expr]

  case class IdentExpr(ident: String)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends Expr

  object IdentExpr extends ParserBridgeSymPos1[String, Expr]

  sealed trait UnopExpr extends Expr

  case class NotExpr(e: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends UnopExpr

  object NotExpr extends ParserBridgeSymPos1[Expr, UnopExpr]

  case class NegExpr(e: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends UnopExpr

  object NegExpr extends ParserBridgeSymPos1[Expr, UnopExpr]

  case class LenExpr(e: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends UnopExpr

  object LenExpr extends ParserBridgeSymPos1[Expr, UnopExpr]

  case class OrdExpr(e: Expr)(val pos: (Int, Int)) extends UnopExpr

  object OrdExpr extends ParserBridgePos1[Expr, UnopExpr]

  case class ChrExpr(e: Expr)(val pos: (Int, Int)) extends UnopExpr

  object ChrExpr extends ParserBridgePos1[Expr, UnopExpr]

  sealed trait BinopExpr extends Expr

  case class MulExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object MulExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class DivExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object DivExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class ModExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object ModExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class AddExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object AddExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class SubExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object SubExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class GTExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object GTExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class GTEQExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object GTEQExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class LTExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object LTExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class LTEQExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object LTEQExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class EQExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object EQExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class NEQExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object NEQExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class AndExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object AndExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]

  case class OrExpr(e1: Expr, e2: Expr)(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends BinopExpr

  object OrExpr extends ParserBridgeSymPos2[Expr, Expr, BinopExpr]


  sealed trait PairElem extends LValue with RValue

  case class Fst(lvalue: LValue)(val pos: (Int, Int))(var ty : SemTypes.SemType) extends PairElem

  object Fst extends ParserBridgePos1[LValue, PairElem] {
    override def apply(x: LValue)(pos: (Int, Int)): PairElem = {
      new Fst(x)(pos)(InternalPairSemType)
    }
  }

  case class Snd(lvalue: LValue)(val pos: (Int, Int))(var ty : SemTypes.SemType) extends PairElem

  object Snd extends ParserBridgePos1[LValue, PairElem] {
    override def apply(x: LValue)(pos: (Int, Int)): PairElem = new Snd(x)(pos)(InternalPairSemType)
  }

  // Statement branch of AST
  sealed trait Statement extends AST

  case object Skip extends Statement with ParserBridge0[Statement]

  case class VarDec(assignType: Type, ident: String, rvalue: RValue)(var symbolTable: Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends Statement

  object VarDec extends ParserBridgeSymPos3[Type, String, RValue, Statement]

  case class Assign(lvalue: LValue, rvalue: RValue)(val pos: (Int, Int)) extends Statement

  object Assign extends ParserBridgePos2[LValue, RValue, Statement]

  case class Read(lvalue: LValue)(var symbolTable: Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends Statement

  object Read extends ParserBridgeSymPos1[LValue, Statement]

  case class Free(e: Expr)(var symbolTable: Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends Statement

  object Free extends ParserBridgeSymPos1[Expr, Statement]

  case class Return(e: Expr)(val pos: (Int, Int)) extends Statement

  object Return extends ParserBridgePos1[Expr, Statement]

  case class Exit(e: Expr)(val pos: (Int, Int)) extends Statement

  object Exit extends ParserBridgePos1[Expr, Statement]

  case class Print(e: Expr, var expType : Option[SemTypes.SemType] = None)(var symbolTable: Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends Statement

  object Print extends ParserBridgeSymPos1[Expr, Statement] {
    override def apply(x: Expr)(symbolTable: Option[SymbolTable[SemTypes.SemType]], pos: (Int, Int)): Statement = {
      Print(x, None)(symbolTable, pos)
    }
  }

  case class Println(e: Expr, var expType : Option[SemTypes.SemType] = None)(var symbolTable: Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends Statement

  object Println extends ParserBridgeSymPos1[Expr, Statement] {
    override def apply(x: Expr)(symbolTable: Option[SymbolTable[SemTypes.SemType]], pos: (Int, Int)): Statement = {
      Println(x, None)(symbolTable, pos)
    }
  }

  case class If(cond: Expr, thenStat: Statement, elseStat: Statement)(val pos: (Int, Int)) extends Statement

  object If extends ParserBridgePos3[Expr, Statement, Statement, Statement]

  case class While(cond: Expr, doStat: Statement)(val pos: (Int, Int)) extends Statement

  object While extends ParserBridgePos2[Expr, Statement, Statement]

  case class ScopeStat(stat: Statement)(val pos: (Int, Int)) extends Statement

  object ScopeStat extends ParserBridgePos1[Statement, ScopeStat]


  case class ConsecStat(first: Statement, next: Statement) extends Statement

  // LValue branch of AST
  sealed trait LValue extends AST

  case class IdentValue(s: String)(var st: Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends LValue

  object IdentValue extends ParserBridgeSymPos1[String, LValue]

  case class ArrayElem(ident: String, exprs: List[Expr])(var st : Option[SymbolTable[SemTypes.SemType]], val pos: (Int, Int)) extends LValue with Expr

  object ArrayElem extends ParserBridgeSymPos2[String, List[Expr], ArrayElem]

  // RValue branch of AST
  sealed trait RValue extends AST

  case class ArrayLiter(exprs: List[Expr])(val pos: (Int, Int)) extends RValue

  object ArrayLiter extends ParserBridgePos1[List[Expr], RValue]

  case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends RValue

  object NewPair extends ParserBridgePos2[Expr, Expr, RValue]

  case class Call(ident: String, args: List[Expr])(val pos: (Int, Int)) extends RValue

  object Call extends ParserBridgePos2[String, List[Expr], RValue]
}
