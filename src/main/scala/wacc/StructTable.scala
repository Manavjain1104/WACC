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

  class StructDef() extends GenericTable[SemType] {

    var structSize = 0
    private val offsetMap: mutable.Map[String, Int] = mutable.Map.empty[String, Int]
    private val typeMap: mutable.Map[String, SemType] = mutable.Map.empty[String, SemType]
    private val keys : mutable.ListBuffer[String] = mutable.ListBuffer.empty[String]

    def addOffset(field: String, offset: Int) = offsetMap(field) = offset

    def getOffset(field: String) = offsetMap.get(field)

    def add(field: String, tType: SemType) = {
      typeMap(field) = tType
      keys.append(field)
    }

    def getKeys(): List[String] = keys.toList

    override def lookup(name: String): Option[SemType] = typeMap.get(name)

    override def lookupAll(name: String): Option[SemType] = {
      assert(assertion = false, "LookupAll executed in Struct table")
      None
    }

    override def getNestedEntries(): Int = {
      assert(assertion = false, "getNestedEntries executed in Struct table")
      0
    }

    override def getParent(): Option[GenericTable[SemType]] = {
      assert(assertion = false, "LookupAll executed in Struct table")
      None
    }

    override def incrementCount(): Unit = {
      assert(assertion = false, "LookupAll executed in Struct table")
    }

    override def getCount(): Int = {
      assert(assertion = false, "LookupAll executed in Struct table")
      -1
    }
  }

  class StructTable() {

    private val structDefMap: mutable.Map[String, StructDef] = mutable.Map.empty[String, StructDef]

    def lookup(name: String): Option[StructDef] = structDefMap.get(name)

    def add(name: String): Unit = structDefMap.addOne(name, new StructDef())

    def getKeys() : Iterable[String] = structDefMap.keys
  }
}
