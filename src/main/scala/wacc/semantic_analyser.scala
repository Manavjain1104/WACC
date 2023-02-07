package wacc

import wacc.AST._
import wacc.SemTypes._
import wacc.error._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class semantic_analyser {

  // error errorLog for semantic analysis
  private final val errorLog = mutable.ListBuffer.empty[SemanticError]

  // to store function return type in the intermediate scope
  private final val ENCLOSING_FUNC_RETURN_TYPE = "?_returnType"

  def checkProgram(program : Program, topLevelSymbolTable: SymbolTable): Option[ListBuffer[SemanticError]] = {

    // first pass to develop function names on the top level symbol table
    val funcDefinitions = mutable.Set.empty[Func]
    for (func <- program.funcs) {
      if (topLevelSymbolTable.lookup("$" + func.ident).isEmpty) {
        topLevelSymbolTable.add("$" + func.ident, convertToSem(func))
        funcDefinitions.add(func)
      } else {
        println("Func: " + func.ident + " defined twice")
        errorLog += DuplicateIdentifier(func.pos, func.ident, Some("Duplicate function definition."))
      }
    }

    for (func <- funcDefinitions) {
      checkFunction(func, topLevelSymbolTable)
    }

    checkStatement(program.stat, topLevelSymbolTable)

    if (errorLog.isEmpty) {
      None
    } else {
      Some(errorLog)
    }
  }

  private def convertToSem(ty: Type): SemType = {
    ty match {
      case IntType() => IntSemType
      case BoolType() => BoolSemType
      case CharType() => CharSemType
      case StringType() => StringSemType
      case ArrayType(t : Type) => ArraySemType(convertToSem(t))
      case PairType(pt1, pt2) => PairSemType(convertToSem(pt1), convertToSem(pt2))
      case _ => throw new RuntimeException("Should not reach here")
    }
  }

  private def convertToSem(pty: PairElemType): SemType = {
    pty match{
      case IntType() => IntSemType
      case BoolType() => BoolSemType
      case CharType() => CharSemType
      case StringType() => StringSemType
      case ArrayType(t : Type) => ArraySemType(convertToSem(t))
      case DummyPair => PairSemType(InternalPairSemType, InternalPairSemType)
      case _ => throw new RuntimeException("Should not reach here")
    }
  }
  private def convertToSem(func: Func): SemType = {
    FuncSemType(convertToSem(func.retType),
      func.params.map(param => convertToSem(param.paramType)),
      func.params.length)
  }
  private def matchTypes(type1: SemType, type2: SemType): Boolean = {
    println("matchTypes got ", type1, type2)
    type1 match {
      case InternalPairSemType => {
        println("t1 was ", InternalPairSemType)
        true
      }
      case FuncSemType(_, _, _) => {
        println("t1 was function")
        false
      }
      case ArraySemType(t1) => {
        println("t1 was array sem")
        type2 match {
          case ArraySemType(t2) => matchTypes(t1, t2)
          case InternalPairSemType => true
          case _ => false
        }
      }
      case PairSemType(pt1, pt2) => {
        println("t1 was pair sem")
        type2 match {
          case PairSemType(t1, t2) => matchTypes(pt1, t1) && matchTypes(pt2, t2)
          case InternalPairSemType => true
          case _ => false
        }
      }
      case _ => {
        println("t1 was base type")
        matchBaseTypes(type1, type2)
      }
    }
  }

  private def matchBaseTypes(t1: SemType, t2: SemType): Boolean = {
    t1 match {
      case BoolSemType => {
        t2 match {
          case BoolSemType => true
          case InternalPairSemType => true
          case _ => false
        }
      }
      case CharSemType => {
        t2 match {
          case CharSemType => true
          case InternalPairSemType => true
          case _ => false
        }
      }
      case IntSemType => {
        t2 match {
          case IntSemType => true
          case InternalPairSemType => true
          case _ => false
        }
      }
      case StringSemType => {
        t2 match {
          case StringSemType => true
          case InternalPairSemType => true
          case _ => false
        }
      }
    }
  }

  private def checkExpr(expr: Expr, symbolTable: SymbolTable): Option[SemType] = {
    println("In CheckExpr")
    expr match {
      // atomic expressions
      case IntExpr(_) => Some(IntSemType)
      case CharExpr(_) => Some(CharSemType)
      case StringExpr(_) => Some(StringSemType)
      case BoolExpr(_) => Some(BoolSemType)
      case IdentExpr(ident) => symbolTable.lookupAll(ident)
      case PairExpr => Some(InternalPairSemType)
      case arrayElem: ArrayElem => checkArrayElem(arrayElem, symbolTable)

      // unary operator expressions
      case NotExpr(e: Expr) => {
        val opType: Option[SemType] = checkExpr(e, symbolTable)
        if (opType.isDefined && matchTypes(opType.get, BoolSemType)) return opType
        println("NotOpType Issue")
        None
      }
      case NegExpr(e: Expr) => {
        val opType: Option[SemType] = checkExpr(e, symbolTable)
        if (opType.isDefined && matchTypes(opType.get, IntSemType)) return opType
        println("NegOpType Issue")
        None
      }
      case LenExpr(e: Expr) => {
        val opType: Option[SemType] = checkExpr(e, symbolTable)
        println(e)
        if (opType.isDefined) {
          opType.get match {
            case ArraySemType(_) => return Some(IntSemType)
            case t => return None// TypeError(pos, expectedType ArraySemType, foundType t, varName e)
          }
        }
        println("LenOpType Issue")
        None
      }
      case ChrExpr(e: Expr) => {
        val opType: Option[SemType] = checkExpr(e, symbolTable)
        if (opType.isDefined) {
          opType.get match {
            case IntSemType => return Some(CharSemType)
            case t => return None// TypeError(pos, expectedType IntSemType, foundType t, varName e)
          }
        }
        println("ChrOpType Issue")
        None
      }
      case OrdExpr(e: Expr) => {
        val opType: Option[SemType] = checkExpr(e, symbolTable)
        if (opType.isDefined) {
          opType.get match {
            case CharSemType => return Some(IntSemType)
            case t => return None// TypeError(pos, expectedType CharSemType, foundType t, varName e)
          }
        }
        None
      }

      // Binary operator expressions
      case ModExpr(e1, e2) => checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case MulExpr(e1, e2) => checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case DivExpr(e1, e2) => checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case AddExpr(e1, e2) => checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case SubExpr(e1, e2) => checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case AndExpr(e1, e2) => checkBinOpWithType(e1, e2, symbolTable, BoolSemType)
      case OrExpr(e1, e2) => checkBinOpWithType(e1, e2, symbolTable, BoolSemType)

      case GTExpr(e1, e2) => checkComparisonBinop(e1, e2, symbolTable)
      case GTEQExpr(e1, e2) => checkComparisonBinop(e1, e2, symbolTable)
      case LTExpr(e1, e2) => checkComparisonBinop(e1, e2, symbolTable)
      case LTEQExpr(e1, e2) => checkComparisonBinop(e1, e2, symbolTable)

      case EQExpr(e1, e2) => checkEqBinOp(e1, e2, symbolTable)
      case NEQExpr(e1, e2) => checkEqBinOp(e1, e2, symbolTable)

      case _ => println("Should not reach here")
        None
    }
  }

  private def checkEqBinOp(e1 : Expr, e2 : Expr, symbolTable: SymbolTable): Option[SemType] = {
    val e1Type: Option[SemType] = checkExpr(e1, symbolTable)
    if (e1Type.isDefined) {
      val e2Type: Option[SemType] = checkExpr(e2, symbolTable)
      if (e2Type.isDefined) {
        if (matchTypes(e1Type.get, e2Type.get)) {
          return Some(BoolSemType)
        }
      }
    }
    None
  }

  private def checkComparisonBinop(e1 : Expr, e2 : Expr, symbolTable: SymbolTable): Option[SemType] = {
    val e1Type: Option[SemType] = checkExpr(e1, symbolTable)
    if (e1Type.isDefined) {
      val e2Type: Option[SemType] = checkExpr(e2, symbolTable)
      if (e2Type.isDefined) {
        if ((matchTypes(IntSemType, e1Type.get) && matchTypes(IntSemType, e2Type.get)) ||
            (matchTypes(CharSemType, e1Type.get) && matchTypes(CharSemType, e2Type.get))) {
          return Some(BoolSemType)
        }
      }
    }
    None
  }

  private def checkBinOpWithType(e1 : Expr, e2 : Expr,
                         symbolTable: SymbolTable, matchBaseType : SemType): Option[SemType] = {
    val e1Type: Option[SemType] = checkExpr(e1, symbolTable)
    if (e1Type.isDefined) {
      val e2Type: Option[SemType] = checkExpr(e2, symbolTable)
      if (e2Type.isDefined) {
        if (matchTypes(matchBaseType, e1Type.get) && matchTypes(matchBaseType, e2Type.get)) {
          return Some(matchBaseType)
        }
      }
    }
    None
  }

  private def checkRvalue(rvalue: RValue, symbolTable: SymbolTable): Option[SemType] = {
    rvalue match {
      case expr: Expr => checkExpr(expr, symbolTable)
      case arrayLiter: ArrayLiter => checkArrayLiteral(arrayLiter, symbolTable)
      case NewPair(e1: Expr, e2: Expr)=> {
        val e1Type: Option[SemType] = checkExpr(e1, symbolTable)
        if (e1Type.isDefined) {
          val e2Type: Option[SemType] = checkExpr(e2, symbolTable)
          if (e2Type.isDefined) {
            return Some(PairSemType(e1Type.get, e2Type.get))
          }
          //UnknownIdentifierError(pos, e2Type, "variable"?)
        }
        None // UnknownIdentifierError(pos, e1Type, "variable"?)
      }
      case Call(ident, args) => {
        // valid function in symbol table
        val identSemType = symbolTable.lookupAll("$" + ident)
        if (identSemType.isEmpty) {
          println(ident + " : function not found! 1")
          return None // UnknownIdentifierError(pos, ident, "function")
        }
        println("matching + " + ident)
        println(identSemType.get)
        identSemType.get match {
          case functype: FuncSemType => {
            println("func case")
            // parameters length match
            if (functype.numParams != args.length) {
              println("argument lengths dont match")
              return None // ArityMismatch(pos, functype.numParams, args.length, "wrong number of function arguments")
            }

            // parameters and arguments type match
            for (i <- args.indices) {
              val expType = checkExpr(args(i), symbolTable)
              if (expType.isEmpty) {
                println("1@")
                return None
              }
              if (!matchTypes(expType.get, functype.paramTypes(i))) {
                println("2@")
                return None
              }
            }
            println("returning")
            Some(functype.retType)
          }
          case _ => None // err tried calling variable!
          //UnknownIdentifierError(pos, identSemType.get, can only call functions)
        }
      }

      case elem: PairElem => checkPairElem(elem, symbolTable)
      case _ => println("Should not reach here")
        None
    }
  }
  private def checkLvalue(lvalue: LValue, symbolTable: SymbolTable): Option[SemType] = {
    lvalue match {
      case IdentValue(name: String) => {
        symbolTable.lookupAll(name)
      }
      case arrayElem: ArrayElem => checkArrayElem(arrayElem, symbolTable)
      case elem: PairElem => checkPairElem(elem, symbolTable)
    }
  }
  private def checkArrayElem(arrayElem: ArrayElem, symbolTable : SymbolTable): Option[SemType] = {
    val identType: Option[SemType] = symbolTable.lookupAll(arrayElem.ident)
    if (identType.isDefined) {
      var arrayTypeHolder: SemType = identType.get
      var i = 0
      while (i < arrayElem.exprs.length) {
        arrayTypeHolder match {
          case ArraySemType(t) => {
            val indexType: Option[SemType] = checkExpr(arrayElem.exprs(i), symbolTable)
            if (indexType.isEmpty || !matchTypes(indexType.get, IntSemType)) {
              println("Index type was not Int")
              return None // TypeError(pos, indexType.get or null if indexType.isEmpty, IntSemType, arrays must have int indexes)
            }
            arrayTypeHolder = t
          }
          case _ => {
            println("Array Elem ", arrayElem, " does not have correct depth")
            return None
          }
        }
        i = i + 1
      }
      return Some(arrayTypeHolder)
    }
    println("array elem identifier " + arrayElem.ident + " not found")
    None // UnknownIdentifierError(pos, arrayElem.ident, identifier is undefined
  }
  private def checkPairElem(pe : PairElem, symbolTable: SymbolTable) : Option[SemType] = {
    var is_fst: Boolean = false
    val insideLval: LValue = pe match {
      case Fst(lvalue) => {
        is_fst = true
        lvalue
      }
      case Snd(lvalue) => lvalue
    }
    insideLval match {
      case elem: PairElem => {
        if (checkPairElem(elem, symbolTable).isDefined) {
          return Some(InternalPairSemType)
        }
        None
      }
      case arrayElem: ArrayElem => {
        val opArrayItemType= checkArrayElem(arrayElem, symbolTable)
        if (opArrayItemType.isDefined) {
          opArrayItemType.get match {
            case PairSemType(pt1, pt2) => {
              if (is_fst) {
                return Some(pt1)
              } else {
                return Some(pt2)
              }
            }
            case InternalPairSemType => Some(InternalPairSemType)
            case t => {
              println("cant do fst/snd of non pair types")
              return None
              //TypeError(pos, PairType, t, can only call fst or snd on pairs)
            }
          }
        }
        None
      } // cant do fst or snd of ArrayElem
      case IdentValue(s) => {
        val identType: Option[SemType] = symbolTable.lookupAll(s)
        if (identType.isDefined) {
          identType.get match {
            case PairSemType(pt1, pt2) => {
              if (is_fst) {
                return Some(pt1)
              } else {
                return Some(pt2)
              }
            }
            case _ => None //TypeError(pos, PairType, t, can only call fst or snd on pairs)
          }
        }
        None //UnknownIdentifierError(pos, identType.get, identType isn't defined)
      }
    }
  }
  private def checkFunction(func: Func, symbolTable: SymbolTable): Option[SemType] = {
    val intermediateTable = new SymbolTable(Some(symbolTable))
    if (!checkParams(func.params, intermediateTable, mutable.Set.empty[String])) {
      None
    } else {
      val funcSemType: SemType = convertToSem(func.retType)
      intermediateTable.add(ENCLOSING_FUNC_RETURN_TYPE, funcSemType)
//        symbolTable.add(func.ident, funcSemType) this will be done in the first pass in checkProgram
      val childSym = new SymbolTable(Some(intermediateTable))
      if (checkStatement(func.stat, childSym).isDefined) {
        func.st = Some(childSym)
        return Some(funcSemType)
      }
      None
    }
  }

  private def checkStatement(node: Statement, symbolTable: SymbolTable): Option[SemType] = {
      node match {
        case Skip => Option(InternalPairSemType) // not an error here

        case VarDec(assignType, ident, rvalue) => {
          println("Start VarDec of ", ident)
          if (symbolTable.lookup(ident).isDefined) {
            println("Cannot have duplicate variable names")
            None //DuplicateIdentifier(pos, ident, variable)
          } else {
            val rvalType: Option[SemType] = checkRvalue(rvalue, symbolTable)
            if (rvalType.isEmpty) {
              None
            } else {
              val assignSemType = convertToSem(assignType)
              if (!matchTypes(assignSemType, rvalType.get)) {
                None
              } else {
                symbolTable.add(ident, assignSemType)
                println("End VarDec of ", ident)
                Some(assignSemType)
              }
            }
          }
        }

        case Assign(lvalue, rvalue) => {
          val oplvalSemType: Option[SemType] = checkLvalue(lvalue, symbolTable)
          if (oplvalSemType.isEmpty) {
            println("lvalue assignment identifier not found", lvalue, rvalue)
            None // TypeError(pos, oplvalSemType, none, oplvalSemType is empty)
          } else {
            oplvalSemType.get match {
              case FuncSemType(_, _, _) => {
                println("Can't assign to a function")
                None
              } // TypeError(pos, ident/pair-elem/array-elem, function, function cant be lvalue of assignment
              case lvalSemType: SemType => {

                // there are 5 rvalue cases
                rvalue match {
                  case rval@ArrayLiter(_) => {
                    val arrayType: Option[SemType] = checkArrayLiteral(rval, symbolTable)
                    if (arrayType.isDefined) {
                      if (matchTypes(lvalSemType, arrayType.get)) {
                        return arrayType
                      }
                    }
                    None
                  }

                  case rCall@Call(ident, _) => {
                    val opFuncRetType: Option[SemType] = checkRvalue(rCall, symbolTable)
                    if (opFuncRetType.isDefined) {
                      if (matchTypes(opFuncRetType.get, lvalSemType)) {
                        return opFuncRetType
                      }
                    }
                    println("Call " + ident, " had issues")
                    None
                  }

                  case NewPair(expr1, expr2) => {
                    val pt: PairSemType = lvalSemType match {
                      case p@PairSemType(pt1, pt2) => p
                      case t => return None // TypeError(pos, PairType, t, lhs is not pair type)
                    }
                    val expr1Type = checkExpr(expr1, symbolTable)
                    if (expr1Type.isEmpty) {
                      return None // TypeError(pos, some expr, none, expr is empty)
                    }
                    val expr2Type = checkExpr(expr2, symbolTable)
                    if (expr2Type.isEmpty) {
                      return None // TypeError(pos, some expr, none, expr is empty)
                    }

                    if (!matchTypes(expr1Type.get, pt.pt1) || !matchTypes(expr2Type.get, pt.pt2)) {
                      None // TypeError(pos, pair-elem-type, pt.pt1 or pt.pt2 (whichever fails), expression type mismatch)
                    } else Some(pt)
                  }

                  case Fst(rhsLVal) => checkPairElemAssign(rhsLVal, lvalSemType, symbolTable, is_fst = true)
                  case Snd(rhsLVal) => checkPairElemAssign(rhsLVal, lvalSemType, symbolTable, is_fst = false)

                  case expr: Expr => {
                    println("Start Expr assignment", lvalue, expr)
                    val exprSemType: Option[SemType] = checkExpr(expr, symbolTable)
                    if (exprSemType.isDefined) {
                      if (matchTypes(exprSemType.get, lvalSemType)) {
                        println("End Expr assignment", lvalue, expr)
                        return Some(lvalSemType)
                      }
                    }
                    None
                  }
                  case _ => println("Error while parsing shld not get here")
                    None
                }
              }
              case t => None // TypeError(pos, SemType, t, expected rvalue SemType)
            }
          }
        }

        case read: Read => {
          val readLvalSemType: Option[SemType] = checkLvalue(read.lvalue, symbolTable)
          if (readLvalSemType.isDefined) {
            read.symbolTable = Some(symbolTable)
            readLvalSemType.get match {
              case IntSemType => return Some(IntSemType)
              case CharSemType => return Some(CharSemType)
              case InternalPairSemType => return None // special err due to type erasure
              case _ => return None // err cant read to pairs/funcs etc
            }
          }
          None
        }

        case Free(expr: Expr) => {
          val exprType = checkExpr(expr, symbolTable)
          if (exprType.isDefined) {
            exprType.get match {
              case _: PairSemType => exprType
              case _: ArraySemType => exprType
              case _ => None // err
            }
          } else {
            None // err
          }
        }

        case Return(expr: Expr) => {
          val funcRetType: Option[SemType] = symbolTable.lookupAll(ENCLOSING_FUNC_RETURN_TYPE)
          if (funcRetType.isDefined) {
            val exprType: Option[SemType] = checkExpr(expr, symbolTable)
            if (exprType.isDefined) {
              if (matchTypes(exprType.get, funcRetType.get)) {
                println("RETURN", exprType.get, funcRetType.get)
                return exprType
              }
            }
          }
          println("Main function cannot have return. Return must exist in a function body!")
          None
        }

        case Exit(expr: Expr) => {
          val exprType: Option[SemType] = checkExpr(expr, symbolTable)
          if (exprType.isDefined) {
            exprType.get match {
              case IntSemType => return exprType
              case _ => {
                println("Cannot exit with a non integer exit code")
                return None
              }
            }
          }
          None
        }

        case printStat@Print(expr: Expr) => {
          printStat.symbolTable =  Some(symbolTable)
          checkExpr(expr, symbolTable) // TODO check for none
        }

        case printlnStat@Println(expr: Expr) => {
          printlnStat.symbolTable =  Some(symbolTable)
          checkExpr(expr, symbolTable) // TODO check for none
        }
        case If(cond, thenStat, elseStat) => {
          val condType: Option[SemType] = checkExpr(cond, symbolTable)
          println("hehehe1")
          if (condType.isDefined) {
            println("hehehe2")
            if (matchTypes(condType.get, BoolSemType)) {
              val thenScope = new SymbolTable(Some(symbolTable))
              if (checkStatement(thenStat, thenScope).isDefined) {
                println("then was good")
                val elseScope = new SymbolTable(Some(symbolTable))
                return checkStatement(elseStat, elseScope)
              }
            }
          }
          None
        }

        case While(cond, doStat) => {
          val condType = checkExpr(cond, symbolTable)
          if (condType.isDefined) {
            if (matchTypes(condType.get, BoolSemType)) {
              val doScope = new SymbolTable(Some(symbolTable))
              val statType = checkStatement(doStat, doScope) // TODO check for none
              if (statType.isDefined) {
                return statType
              }
            }
          }
          None
        }

        case ScopeStat(stat) => {
          val newScope = new SymbolTable(Some(symbolTable))
          val statType = checkStatement(stat, newScope)
          if (statType.isEmpty) {
            println("this stat is wrong ", stat)
            return None
          }
          statType
        }

        case ConsecStat(first, next) => {
          if (checkStatement(first, symbolTable).isDefined) {
            return checkStatement(next, symbolTable)
          }
          println("this stat : is wrong ", first)
          None
        }
      }
    }

  private def checkParams(params: List[Param], symbolTable: SymbolTable, paramNames: mutable.Set[String]): Boolean = {
      for (param <- params) {
        if (paramNames.contains(param.ident)) {
          return false // duplicate func names
        } else {
          paramNames.add(param.ident)
          symbolTable.add(param.ident, convertToSem(param.paramType))
        }
      }
      true
    }
  private def isConcrete(pt1: SemType) : Boolean = pt1 != InternalPairSemType
  private def checkPairElemAssign(rhsLval: LValue, lvalSemType: SemType, symbolTable: SymbolTable, is_fst: Boolean): Option[SemType] = {
      println("kun faya ")
      val rhsLvalType: Option[SemType] = checkLvalue(rhsLval, symbolTable)
      if (rhsLvalType.isEmpty) {
        println("fst/snd lvalue ", rhsLval , " failed")
        return None
      }
      rhsLvalType.get match {
        case t@PairSemType(pt1, pt2) => {
          println(rhsLval, "has type ", t)
          println(lvalSemType, " < ---- lvalsemType")
          if (is_fst) {
            if (matchTypes(pt1, lvalSemType)) {
              if (!isConcrete(lvalSemType) && !isConcrete(pt1)) {
                println("cant have Internal Pair type on both")
                return None // cant have Internal Pair type on both, TypeError
              }
              else return Some(lvalSemType)
            }
          } else {
            if (matchTypes(pt2, lvalSemType)) {
              if (!isConcrete(lvalSemType) && !isConcrete(pt2)) {
                println("cant have Internal Pair type on both")
                return None // cant have Internal Pair type on both, TypeError
              }
              else return Some(lvalSemType)
            }
          }
          println("should not reach here")
          None
        }
        case _ => {
          println("err fst applied to non pair type")
          None
        }
      }
    }

  private def checkArrayLiteral(arraylit: ArrayLiter, symbolTable: SymbolTable): Option[SemType] = {
      if (arraylit.exprs.nonEmpty) {
        val expType = checkExpr(arraylit.exprs.head, symbolTable)
        if (expType.isEmpty) {
          println("Head expr of ", arraylit, " could not be evaluated")
          return Some(ArraySemType(InternalPairSemType))
        } else {
          for (expr <- arraylit.exprs.tail) {
            val expTypeRest = checkExpr(expr, symbolTable)
            if (expTypeRest.isEmpty) {
              println("Expr ", expr, "of ", arraylit, " could not be evaluated")
              return Some(ArraySemType(InternalPairSemType))
            } else {
              if (!matchTypes(expType.get, expTypeRest.get)) {
                println(arraylit, " has different types of elements")
                return None // TypeError(pos, expTypeRest.get, , arraylit elems of different types)
              }
            }
          }
          return Some(ArraySemType(expType.get))
        }
      }
      Some(ArraySemType(InternalPairSemType))
    }
}
