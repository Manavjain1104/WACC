package wacc

import wacc.AST.AST
import collection.mutable.Map

class SymbolTable(val encSymTable: Option[SymbolTable], val map: Map[String, AST]) {

  def add(name : String, node : AST) = {
    map.addOne(name, node)
  }

  def lookup(name :String): Option[AST] = map.get(name)

  def lookupAll(name : String) : Option[AST] = {
    var table : Option[SymbolTable] = Option(this)

    while (table.isDefined) {
      val opVal = table.get.lookup(name)
      if (opVal.isDefined) {
        return opVal
      }
      table = table.get.encSymTable
    }
    None
  }
}
