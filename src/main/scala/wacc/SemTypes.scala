package wacc

object SemTypes {

  // WACC SEMANTIC type hierarchy
  trait SemType

  case object VoidSemType extends SemType
  case object IntSemType extends SemType
  case object BoolSemType extends SemType
  case object CharSemType extends SemType
  case object StringSemType extends SemType


  case class StructSemType(structName: String) extends SemType
  case class ClassSemType(className: String) extends SemType

  case class ArraySemType(t : SemType) extends SemType

  case class PairSemType(pt1 : SemType, pt2 : SemType) extends SemType
  case object InternalPairSemType extends SemType

  case class FuncSemType(retType : SemType, paramTypes : List[SemType], numParams : Int) extends SemType

  case class IfExprSemType(cond : SemType, thenExpr : SemType, elseExpr : SemType) extends SemType
}
