//package wacc
//
//import wacc.AST.{Type, _}
//
//import scala.collection.mutable
//
//class semantic_analyser {
//
//  def matchTypes(type1 : Type, type2: Type): Boolean = {
//    return false // TODO
//  }
//
//  def checkExpr(expr : Expr, symbolTable: SymbolTable) : Option[Type] = {
//    return null // TODO
//  }
//
//  def checkRvalue(rvalue: RValue, symbolTable: SymbolTable): Option[Type] = {
//    return None // TODO
//  }
//
//  def checkFunction(node: AST, symbolTable: SymbolTable) : Option[Type] = {
//    if (symbolTable.lookupAll(ident) != null) {
//      println("Cannot have duplicate function names") // new Err & append to list
//      return false
//    }
//    val paramNames = mutable.Set.empty[String]
//    val intermediateTable = new SymbolTable(symbolTable, mutable.Map.empty)
//    for (param <- params) {
//      if (!checkParam(param, intermediateTable, paramNames)) {
//        return false
//      }
//    }
//    symbolTable.add(ident, node)
//    val newSt = new SymbolTable(intermediateTable, mutable.Map.empty)
//    check(stat, newSt)
//  }
//
//  def check(node: AST, symbolTable: SymbolTable): Boolean = {
//    node match {
//      // checking statements
//      case Skip => true
//      case VarDec(assignType, ident, rvalue) => {
//        if (symbolTable.lookup(ident).isDefined) {
//          return false
//        }
//        val rvalType = checkRvalue(rvalue, symbolTable)
//        if (rvalType == null) {
//          return false
//        }
//        if (!matchTypes(assignType, rvalType)) {
//          return false
//        }
//        symbolTable.add(ident, assignType)
//        true
//      }
//      case Assign(lvalue, rvalue) => {
//
//        val identASTType = symbolTable.lookupAll(getIdentifier(lvalue))
//        if (identASTType == null || identASTType.isInstanceOf[Func]) {
//          return false
//        }
//        val identifierType : Type = identASTType.asInstanceOf[Type]
//
//        // there are 5 rvalue cases
//        rvalue match {
//          case ArrayLiter(exprs: List[Expr]) => {
//            val identType : Type = identASTType match {
//              case ArrayType(t: Type) => t
//              case _ => {
//                println("Array Type does not match on assignment")
//                return false
//              }
//            }
//            if (exprs.nonEmpty) {
//              val expType = checkExpr(exprs.head, symbolTable)
//              if (expType == null) {
//                return false
//              }
//              for (expr <- exprs.tail) {
//                val expTypeRest = checkExpr(expr, symbolTable)
//                if (expTypeRest == null) {
//                  return false
//                }
//                if (!matchTypes(expType, expTypeRest)) {
//                  return false
//                }
//              }
//              // expType is static type of array literals
//              if (!matchTypes(expType, identType)) {
//                return false
//              }
//            }
//            true
//          }
//          case Call(ident, args) => {
//
//            // valid function in symbol table
//            val ast = symbolTable.lookupAll(ident)
//            if (ast == null || !ast.isInstanceOf[Func]) {
//              println(ident + " : function not found!")
//              return false
//            }
//
//            // parameters length match
//            val func: Func = ast.asInstanceOf[Func]
//            if (func.params.length != args.length) {
//              return false
//            }
//
//            // return type of function should match
//            if (!matchTypes(func.retType, identifierType)) {
//              return false
//            }
//
//            // parameters and arguments type match
//            for (i <- args.indices) {
//              val expTpye = checkExpr(args(i), symbolTable)
//              if (expTpye == null) {
//                return false
//              }
//              if (!matchTypes(expTpye, func.params(i).paramType)) {
//                return false
//              }
//            }
//            true
//          }
//
//          case NewPair(expr1, expr2) => {return false}
//        }
//      }
//    }
//  }
//
//  def getIdentifier(lvalue: LValue): String = {
//    lvalue match {
//      case IdentValue(s : String) => s
//      case ArrayElem(ident: String, _) => ident
//      case Fst(lvalue : LValue) => getIdentifier(lvalue)
//      case Snd(lvalue : LValue) => getIdentifier(lvalue)
//      case _ => new Exception("Unknown lvalue in getIdentifier")
//      ""
//    }
//  }
//
//  def checkParam(param: Param, symbolTable: SymbolTable, paramNames: mutable.Set[String]): Boolean = {
//      if (paramNames.contains(param.ident)) {
//        return false
//      }
//      symbolTable.add(param.ident, param.paramType)
//      true
//  }
//
//}
