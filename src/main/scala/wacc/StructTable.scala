package wacc

import scala.collection.mutable
import SemTypes._

object StructTable {

  val baseType = Set(IntSemType, BoolSemType, CharSemType, StringSemType)
  private val allowedTypeBuilder = collection.mutable.Set.empty[SemType]
  allowedTypeBuilder.addAll(baseType)
  for (t <- baseType) {
    allowedTypeBuilder.add(ArraySemType(t))
  }
  for (t1 <- baseType) {
    for (t2 <- baseType) {
      allowedTypeBuilder.add(PairSemType(t1, t2))
    }
  }
  val allowedTypes: Set[SemType] = allowedTypeBuilder.toSet

  class StructDef() {

    var structSize = 0
    private val structMap : mutable.Map[String, (SemType, Int)] = mutable.Map.empty[String, (SemType, Int)]
    private val keys : mutable.ListBuffer[String] = mutable.ListBuffer.empty[String]

    def getOffset(field: String): Option[Int] = {
      val opVal = structMap.get(field)
      if (opVal.isDefined) Some(opVal.get._2) else None
    }

    def lookup(field: String) : Option[SemType] = {
      val value = structMap.get(field)
      if (value.isDefined) Some(value.get._1) else None
    }

    def add(field: String, tType: SemType, offset : Int) : Unit = {
      structMap(field) = (tType, offset)
      keys.append(field)
    }

    def getKeys(): List[String] = keys.toList
  }

  class StructTable() {

    private val structDefMap: mutable.Map[String, StructDef] = mutable.Map.empty[String, StructDef]

    def lookup(name: String): Option[StructDef] = structDefMap.get(name)

    def add(name: String): Unit = structDefMap.addOne(name, new StructDef())

    def getKeys() : Iterable[String] = structDefMap.keys
  }
}
