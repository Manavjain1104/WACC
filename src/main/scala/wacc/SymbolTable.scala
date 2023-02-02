package wacc

import wacc.AST.AST
import collection.mutable.Map

class SymbolTable(val encSymTable: SymbolTable, val map: Map[String, AST]) {

  def add(name : String, node : AST) = {
    map.addOne(name, node)
  }

  def lookup(name :String): Option[AST] = map.get(name)

  def lookupAll(name : String) : AST = {
    var table : SymbolTable = this

    while (table != null) {
      val opVal = table.lookup(name)
      if (opVal.isDefined) {
        return opVal.get
      }
      table = table.encSymTable
    }
    null
  }
}
