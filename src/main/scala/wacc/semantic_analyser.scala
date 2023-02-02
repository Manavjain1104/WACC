package wacc

import wacc.AST._

import scala.collection.mutable

class semantic_analyser {

  def checkRvalueAndReturnType(rvalue: RValue, symbolTable: SymbolTable): Type = {
    return null // TODO
  }

  def matchTypes(type1 : Type, type2: Type): Boolean = {
    return false // TODO
  }

  def checkExpr(expr : Expr, symbolTable: SymbolTable) : Type = {
    return null // TODO
  }

  def check(node: AST, symbolTable: SymbolTable): Boolean = {
    node match {
      case Func(returnType, ident, params, stat) => {
        if (symbolTable.lookupAll("ident") != null) {
          println("Cannot have duplicate function names")
          return false
        }
        val paramNames = mutable.Set.empty[String]
        val intermediateTable = new SymbolTable(symbolTable, mutable.Map.empty)
        for (param <- params) {
          if (!checkParam(param, intermediateTable, paramNames)) {
            return false
          }
        }
        symbolTable.add(ident, returnType)
        val newSt = new SymbolTable(intermediateTable, mutable.Map.empty)
        check(stat, newSt)
      }

      case Skip => true
      case VarDec(assignType, ident, rvalue) => {
        if (symbolTable.lookup(ident).isDefined) {
          return false
        }
        matchTypes(assignType, checkRvalueAndReturnType(rvalue, symbolTable))
        symbolTable.add(ident, assignType)
        return true
      }
      case Assign(lvalue, rvalue) => {

      }
    }
  }


  def checkParam(param: Param, symbolTable: SymbolTable, paramNames: mutable.Set[String]): Boolean = {
      if (paramNames.contains(param.ident)) {
        return false
      }
      symbolTable.add(param.ident, param.paramType)
      true
  }

}
