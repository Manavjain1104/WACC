package wacc

import wacc.AST.{Param, Scope}

import scala.collection.mutable.ListBuffer

object ClassTable {

  case class ClassTable() {
    private val classMap = collection.mutable.Map.empty[String, (SymbolTable[SemTypes.SemType], SymbolTable[Scope], ListBuffer[Param])]

    def lookup(name : String) : Option[(SymbolTable[SemTypes.SemType], SymbolTable[Scope], ListBuffer[Param])] = classMap.get(name)
    def add(name : String, typeTable: SymbolTable[SemTypes.SemType], scopeTable: SymbolTable[Scope], param: ListBuffer[Param]) : Unit = classMap.addOne((name, (typeTable, scopeTable, param)))
    def getKeys: Iterable[String] = classMap.keys
  }

}
