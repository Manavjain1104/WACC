package wacc

import wacc.SemTypes.SemType

import scala.collection.mutable

class SymbolTable(val encSymTable: Option[SymbolTable],
                  val map: mutable.Map[String, SemType] = mutable.Map.empty) {

  def add(name: String, ty: SemType) = {
    map.addOne(name, ty)
  }

  def lookup(name: String): Option[SemType] = map.get(name)

  def lookupAll(name: String): Option[SemType] = {
    var table: Option[SymbolTable] = Option(this)

    while (table.isDefined) {
      val optype = table.get.lookup(name)
      if (optype.isDefined) {
        return optype
      }
      table = table.get.encSymTable
    }
    None
  }
}
