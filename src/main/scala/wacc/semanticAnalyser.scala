package wacc

import wacc.AST._
import wacc.SemTypes._
import wacc.error._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class semanticAnalyser {

  // error semanticErrLogs for semantic analysis
  private final val errorLog = mutable.ListBuffer.empty[SemanticError]

  // to store function return type in the intermediate scope
  private final val ENCLOSING_FUNC_RETURN_TYPE = "?_returnType"

  def checkProgram(program: Program, topLevelSymbolTable: SymbolTable): Option[ListBuffer[SemanticError]] = {

    // first pass to develop function names on the top level symbol table
    val funcDefinitions = mutable.Set.empty[Func]
    for (func <- program.funcs) {
      if (topLevelSymbolTable.lookup("$" + func.ident).isEmpty) {
        topLevelSymbolTable.add("$" + func.ident, convertToSem(func))
        funcDefinitions.add(func)
      } else {
        errorLog += DuplicateIdentifier(func.pos, func.ident, Some("Duplicate function definition."))
      }
    }
    for (func <- funcDefinitions) {
      checkFunction(func, topLevelSymbolTable)
    }
    checkStatement(program.stat, topLevelSymbolTable)

    if (errorLog.nonEmpty) {
      return Some(errorLog)
    }
    None
  }

  def getLvalPos(lval: LValue): ((Int, Int), Int) = {
    lval match {
      case ident: IdentValue => (ident.pos, 0)
      case arrayElem: ArrayElem => (arrayElem.pos, 0)
      case pairElem: PairElem =>
        val insideLval = pairElem match {
          case Fst(lvalue) => lvalue
          case Snd(lvalue) => lvalue
        }
        val insidePos: ((Int, Int), Int) = getLvalPos(insideLval)
        (insidePos._1, 3 + insidePos._2)
      case _ =>
        System.err.println("Cannot pattern match lvalue in get lvalue position function")
        ((-1, -1), -1)
    }

  }

  private def convertToSem(func: Func): SemType = {
    FuncSemType(convertToSem(func.retType),
      func.params.map(param => convertToSem(param.paramType)),
      func.params.length)
  }

  private def convertToSem(ty: Type): SemType = {
    ty match {
      case IntType() => IntSemType
      case BoolType() => BoolSemType
      case CharType() => CharSemType
      case StringType() => StringSemType
      case ArrayType(t: Type) => ArraySemType(convertToSem(t))
      case PairType(pt1, pt2) => PairSemType(convertToSem(pt1), convertToSem(pt2))
      case _ => throw new RuntimeException("Should not reach here")
    }
  }

  private def convertToSem(pty: PairElemType): SemType = {
    pty match {
      case IntType() => IntSemType
      case BoolType() => BoolSemType
      case CharType() => CharSemType
      case StringType() => StringSemType
      case ArrayType(t: Type) => ArraySemType(convertToSem(t))
      case DummyPair => PairSemType(InternalPairSemType, InternalPairSemType)
      case _ => throw new RuntimeException("Should not reach here")
    }
  }

  private def matchBaseTypes(t1: SemType, t2: SemType): Boolean = {
    t1 match {
      case BoolSemType =>
        t2 match {
          case BoolSemType => true
          case InternalPairSemType => true
          case _ => false
        }
      case CharSemType =>
        t2 match {
          case CharSemType => true
          case InternalPairSemType => true
          case _ => false
        }
      case IntSemType =>
        t2 match {
          case IntSemType => true
          case InternalPairSemType => true
          case _ => false
        }
      case StringSemType =>
        t2 match {
          case StringSemType => true
          case InternalPairSemType => true
          case _ => false
        }
    }
  }

  private def matchTypes(type1: SemType, type2: SemType): Boolean = {
    type1 match {
      case InternalPairSemType => true
      case FuncSemType(_, _, _) => false
      case ArraySemType(t1) =>
        type2 match {
          case ArraySemType(t2) => matchTypes(t1, t2)
          case InternalPairSemType => true
          case _ => false
        }
      case PairSemType(pt1, pt2) =>
        type2 match {
          case PairSemType(t1, t2) => matchTypes(pt1, t1) && matchTypes(pt2, t2)
          case InternalPairSemType => true
          case _ => false
        }
      case _ => matchBaseTypes(type1, type2)
    }
  }

  private def checkExpr(expr: Expr, symbolTable: SymbolTable): Option[SemType] = {
    expr match {
      // atomic expressions
      case IntExpr(_) => Some(IntSemType)
      case CharExpr(_) => Some(CharSemType)
      case StringExpr(_) => Some(StringSemType)
      case BoolExpr(_) => Some(BoolSemType)
      case id@IdentExpr(ident) =>
        val identType = symbolTable.lookupAll(ident)
        if (identType.isDefined) {
          id.st = Some(symbolTable)
          return identType
        }
        errorLog += UnknownIdentifierError(id.pos, ident, Some("Unknown variable identifier found"))
        Some(InternalPairSemType)
      case PairExpr() => Some(InternalPairSemType)
      case arrayElem: ArrayElem =>
        arrayElem.st = Some(symbolTable)
        checkArrayElem(arrayElem, symbolTable)

      // unary operator expressions
      case node@NotExpr(e: Expr) =>
        val opType: Option[SemType] = checkExpr(e, symbolTable)

        if (matchTypes(opType.get, BoolSemType)) {
          node.st = Some(symbolTable)
          return opType
        }
        val exprPos = getExprPos(e)
        errorLog += new TypeError(exprPos._1,
          Set(BoolSemType),
          opType.get,
          Some("Expected a bool type for not expression"))(exprPos._2)
        Some(InternalPairSemType)

      case node@NegExpr(e: Expr) =>
        val opType: Option[SemType] = checkExpr(e, symbolTable)
        if (matchTypes(opType.get, IntSemType)) {
          node.st = Some(symbolTable)
          return opType
        }
        errorLog += new TypeError(getExprPos(e)._1,
          Set(IntSemType), opType.get, Some("Expected int type for negate expression"))(getExprPos(e)._2)
        Some(InternalPairSemType)

      case node@LenExpr(e: Expr) =>
        val opType: Option[SemType] = checkExpr(e, symbolTable)
        opType.get match {
          case ArraySemType(_) =>
            node.st = Some(symbolTable)
            Some(IntSemType)
          case unexpectedType =>
            val exprPos = getExprPos(e)
            errorLog += new TypeError(exprPos._1,
              Set(ArraySemType(InternalPairSemType)),
              unexpectedType,
              Some("Expected array type for len expression"))(exprPos._2)
            Some(InternalPairSemType)
        }
      case ChrExpr(e: Expr) =>
        val opType: Option[SemType] = checkExpr(e, symbolTable)
        opType.get match {
          case IntSemType => Some(CharSemType)
          case unexpectedType =>
            errorLog += new TypeError(getExprPos(e)._1,
              Set(IntSemType),
              unexpectedType,
              Some("Expected int type for chr expression"))(getExprPos(e)._2)
            Some(InternalPairSemType)
        }

      case OrdExpr(e: Expr) =>
        val opType: Option[SemType] = checkExpr(e, symbolTable)
        opType.get match {
          case CharSemType => Some(IntSemType)
          case unexpectedType =>
            errorLog += new TypeError(getExprPos(e)._1,
              Set(CharSemType),
              unexpectedType,
              Some("Expected char type for ord expression"))(getExprPos(e)._2)
            Some(InternalPairSemType)
        }

      // Binary operator expressions
      case node@ModExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case node@MulExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case node@DivExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case node@AddExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case node@SubExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkBinOpWithType(e1, e2, symbolTable, IntSemType)
      case node@AndExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkBinOpWithType(e1, e2, symbolTable, BoolSemType)
      case node@OrExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkBinOpWithType(e1, e2, symbolTable, BoolSemType)

      case node@GTExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkComparisonBinOp(e1, e2, symbolTable)
      case node@GTEQExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkComparisonBinOp(e1, e2, symbolTable)
      case node@LTExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkComparisonBinOp(e1, e2, symbolTable)
      case node@LTEQExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkComparisonBinOp(e1, e2, symbolTable)

      case node@EQExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkSameType(e1, e2, symbolTable)
      case node@NEQExpr(e1, e2) =>
        node.st = Some(symbolTable)
        checkSameType(e1, e2, symbolTable)

      case _ => System.err.println("Should not reach here")
        Some(InternalPairSemType)
    }
  }

  private def checkSameType(e1: Expr, e2: Expr, symbolTable: SymbolTable): Option[SemType] = {
    val e1Type: Option[SemType] = checkExpr(e1, symbolTable)
    val e2Type: Option[SemType] = checkExpr(e2, symbolTable)
    if (!matchTypes(e1Type.get, e2Type.get)) {
      val exprPos = getExprPos(e2)
      errorLog += new TypeError(exprPos._1,
        Set(e1Type.get),
        e2Type.get,
        Some("Expected same type for both expressions"))(exprPos._2)
      return Some(InternalPairSemType)
    }
    Some(BoolSemType)
  }

  private def checkComparisonBinOp(e1: Expr, e2: Expr, symbolTable: SymbolTable): Option[SemType] = {
    val e1Type: Option[SemType] = checkExpr(e1, symbolTable)
    val e2Type: Option[SemType] = checkExpr(e2, symbolTable)

    val exprPos2 = getExprPos(e2)
    if (matchTypes(IntSemType, e1Type.get)) {
      if (matchTypes(IntSemType, e2Type.get)) {
        return Some(BoolSemType)
      } else {
        errorLog += new TypeError(exprPos2._1,
          Set(IntSemType),
          e2Type.get,
          Some("Expected int expression for comparison"))(exprPos2._2)
        return Some(InternalPairSemType)
      }
    }
    if (matchTypes(CharSemType, e1Type.get)) {
      if (matchTypes(CharSemType, e2Type.get)) {
        return Some(BoolSemType)
      } else {
        errorLog += new TypeError(exprPos2._1,
          Set(CharSemType),
          e2Type.get,
          Some("Expected char expression for comparison"))(exprPos2._2)
        return Some(InternalPairSemType)
      }
    }
    val exprPos1 = getExprPos(e1)
    errorLog += new TypeError(exprPos1._1,
      Set(IntSemType, CharSemType),
      e1Type.get,
      Some("Can only compare int or char expressions"))(exprPos1._2)
    Some(InternalPairSemType)
  }

  private def checkBinOpWithType(e1: Expr, e2: Expr,
                                 symbolTable: SymbolTable, matchBaseType: SemType): Option[SemType] = {
    val e1Type: Option[SemType] = checkExpr(e1, symbolTable)
    val e2Type: Option[SemType] = checkExpr(e2, symbolTable)

    var err = false

    if (!matchTypes(matchBaseType, e1Type.get)) {
      val exprPos = getExprPos(e1)
      errorLog += new TypeError(exprPos._1,
        Set(matchBaseType), e1Type.get,
        Some("Given expr type invalid for this operation"))(exprPos._2)
      err = true
    }
    if (!matchTypes(matchBaseType, e2Type.get)) {
      val exprPos = getExprPos(e2)
      errorLog += new TypeError(exprPos._1,
        Set(matchBaseType), e2Type.get,
        Some("Given expr type invalid for this operation"))(exprPos._2)
      err = true
    }
    if (err) {
      return Some(InternalPairSemType)
    }
    Some(matchBaseType)
  }

  // offset represents number of non whitespace characters to the left of the position that
  // is needed for error printing
  private def getExprPos(expr: Expr): ((Int, Int), Int) = {
    expr match {
      case intLiter: IntExpr => (intLiter.pos, 0)
      case boolLiter: BoolExpr => (boolLiter.pos, 0)
      case charLiter: CharExpr => (charLiter.pos, 0)
      case stringLiter: StringExpr => (stringLiter.pos, 0)
      case pairLiter: PairExpr => (pairLiter.pos, 0)
      case ident: IdentExpr => (ident.pos, 0)
      case arrayElem: ArrayElem => (arrayElem.pos, 0)

      case unOp: UnopExpr =>
        unOp match {
          case NotExpr(e) =>
            val insidePos: ((Int, Int), Int) = getExprPos(e)
            (insidePos._1, 1 + insidePos._2 + 1)
          case NegExpr(e) =>
            val insidePos: ((Int, Int), Int) = getExprPos(e)
            (insidePos._1, 1 + insidePos._2 + 1)
          case LenExpr(e) =>
            val insidePos2: ((Int, Int), Int) = getExprPos(e)
            (insidePos2._1, insidePos2._2 + 3)
          case ChrExpr(e) =>
            val insidePos2: ((Int, Int), Int) = getExprPos(e)
            (insidePos2._1, insidePos2._2 + 3)
          case OrdExpr(e) =>
            val insidePos2: ((Int, Int), Int) = getExprPos(e)
            (insidePos2._1, insidePos2._2 + 3)
        }

      case binOp: BinopExpr =>
        val expressionOffset0 = binOp match {
          case MulExpr(e, _) => e
          case DivExpr(e, _) => e
          case ModExpr(e, _) => e
          case AddExpr(e, _) => e
          case SubExpr(e, _) => e
          case GTExpr(e, _) => e
          case GTEQExpr(e, _) => e
          case LTExpr(e, _) => e
          case LTEQExpr(e, _) => e
          case EQExpr(e, _) => e
          case NEQExpr(e, _) => e
          case AndExpr(e, _) => e
          case OrExpr(e, _) => e
        }
        getExprPos(expressionOffset0)
      case _ =>
        System.err.println("Cannot pattern match expr in get expr position function")
        ((-1, -1), -1)
    }
  }

  private def checkRvalue(rvalue: RValue, symbolTable: SymbolTable): Option[SemType] = {
    rvalue match {
      case expr: Expr => checkExpr(expr, symbolTable)

      case arrayLiter: ArrayLiter => checkArrayLiteral(arrayLiter, symbolTable)

      case NewPair(e1: Expr, e2: Expr) =>
        val e1Type: Option[SemType] = checkExpr(e1, symbolTable)
        val e2Type: Option[SemType] = checkExpr(e2, symbolTable)
        Some(PairSemType(e1Type.get, e2Type.get))

      case call@Call(ident, args) =>
        // valid function in symbol table
        val identSemType = symbolTable.lookupAll("$" + ident)
        if (identSemType.isEmpty) {
          errorLog += UnknownIdentifierError(call.pos, ident, Some("Unknown function identifier found"))
          return Some(InternalPairSemType)
        }

        identSemType.get match {
          case funcType: FuncSemType =>
            // parameters length match
            if (funcType.numParams != args.length) {
              errorLog += ArityMismatch(call.pos,
                funcType.numParams,
                args.length,
                Some("Wrong number of function arguments"))
              return Some(InternalPairSemType)
            }

            // parameters and arguments type match
            for (i <- args.indices) {
              val expType = checkExpr(args(i), symbolTable)
              if (!matchTypes(expType.get, funcType.paramTypes(i))) {
                val argPos = getExprPos(call.args(i))
                errorLog += new TypeError(argPos._1,
                  Set(funcType.paramTypes(i)), expType.get,
                  Some("Argument type does not match with parameter"))(argPos._2)
                return Some(InternalPairSemType)
              }
            }
            Some(funcType.retType)
          case unexpectedType =>
            errorLog += TypeError(call.pos,
              Set(FuncSemType(InternalPairSemType, List.empty, 0)), unexpectedType,
              Some("Cannot call a non function identifier"))
            Some(InternalPairSemType)
        }

      case elem: PairElem => checkPairElem(elem, symbolTable)
    }
  }

  private def checkLvalue(lvalue: LValue, symbolTable: SymbolTable): Option[SemType] = {
    lvalue match {
      case ident@IdentValue(name: String) =>
        val identType = symbolTable.lookupAll(name)
        if (identType.isDefined) {
          return identType
        }
        errorLog += UnknownIdentifierError(ident.pos, ident.s, Some("Unknown variable Identifier found"))
        Some(InternalPairSemType)
      case arrayElem: ArrayElem => checkArrayElem(arrayElem, symbolTable)
      case elem: PairElem => checkPairElem(elem, symbolTable)
    }
  }

  private def checkArrayElem(arrayElem: ArrayElem, symbolTable: SymbolTable): Option[SemType] = {
    val identType: Option[SemType] = symbolTable.lookupAll(arrayElem.ident)
    if (identType.isDefined) {
      var arrayTypeHolder: SemType = identType.get
      var i = 0
      while (i < arrayElem.exprs.length) {
        arrayTypeHolder match {
          case ArraySemType(t) =>
            val indexType: Option[SemType] = checkExpr(arrayElem.exprs(i), symbolTable)
            if (!matchTypes(indexType.get, IntSemType)) {
              errorLog += TypeError(arrayElem.pos,
                Set(IntSemType),
                indexType.get,
                Some("Arrays must be accessed with only int indices"))
              return Some(InternalPairSemType)
            }
            arrayTypeHolder = t
          case unexpectedType =>
            if (i > 0) {
              errorLog += ArrayError(arrayElem.pos, arrayElem.ident, i, Some("Incorrect dimension depth of array"))
              return Some(InternalPairSemType)
            } else {
              errorLog += TypeError(arrayElem.pos,
                Set(ArraySemType(InternalPairSemType)),
                unexpectedType,
                Some("Can't index non array type"))
              return Some(InternalPairSemType)
            }
        }
        i = i + 1
      }
      return Some(arrayTypeHolder)
    }
    errorLog += UnknownIdentifierError(arrayElem.pos, arrayElem.ident, Some("Unknown array identifier found"))
    Some(InternalPairSemType)
  }

  private def checkPairElem(pe: PairElem, symbolTable: SymbolTable): Option[SemType] = {
    var is_fst: Boolean = false
    val insideLval: LValue = pe match {
      case Fst(lvalue) =>
        is_fst = true
        lvalue
      case Snd(lvalue) => lvalue
    }
    insideLval match {
      case _: PairElem =>
        Some(InternalPairSemType)

      case arrayElem: ArrayElem =>
        val opArrayItemType = checkArrayElem(arrayElem, symbolTable)
        opArrayItemType.get match {
          case PairSemType(pt1, pt2) =>
            if (is_fst) {
              Some(pt1)
            } else {
              Some(pt2)
            }
          case InternalPairSemType => Some(InternalPairSemType)
          case unexpectedType =>
            errorLog += TypeError(arrayElem.pos,
              Set(PairSemType(InternalPairSemType, InternalPairSemType)), unexpectedType,
              Some("can only call fst or snd on pairs"))
            Some(InternalPairSemType)
        }

      case ident@IdentValue(name) =>
        val identType: Option[SemType] = symbolTable.lookupAll(name)
        if (identType.isDefined) {
          identType.get match {
            case PairSemType(pt1, pt2) =>
              if (is_fst) {
                return Some(pt1)
              } else {
                return Some(pt2)
              }
            case unexpectedType =>
              errorLog += TypeError(ident.pos,
                Set(PairSemType(InternalPairSemType, InternalPairSemType)), unexpectedType,
                Some("can only call fst or snd on pairs"))
              return Some(InternalPairSemType)
          }
        }
        errorLog += UnknownIdentifierError(ident.pos, ident.s, Some("Unknown variable identifier found"))
        Some(InternalPairSemType)
    }
  }

  private def checkFunction(func: Func, symbolTable: SymbolTable): Unit = {
    val intermediateTable = new SymbolTable(Some(symbolTable))
    if (checkParams(func.params, intermediateTable, mutable.Set.empty[String])) {
      val funcSemType: SemType = convertToSem(func.retType)
      intermediateTable.add(ENCLOSING_FUNC_RETURN_TYPE, funcSemType)
      val childSym = new SymbolTable(Some(intermediateTable))
      if (checkStatement(func.stat, childSym).isDefined) {
        func.st = Some(childSym)
      }
    }
  }

  private def checkStatement(node: Statement, symbolTable: SymbolTable): Option[SemType] = {
    node match {
      case Skip => Some(InternalPairSemType)
      case varDec@VarDec(assignType, ident, rvalue) =>

        if (symbolTable.lookup(ident).isDefined) {
          errorLog += DuplicateIdentifier(varDec.pos, varDec.ident, Some("Duplicate variable identifier found"))
          Some(InternalPairSemType)
        } else {
          val rvalType: Option[SemType] = checkRvalue(rvalue, symbolTable)
          val assignSemType = convertToSem(assignType)
          if (!matchTypes(assignSemType, rvalType.get)) {
            errorLog += TypeError(varDec.pos,
              Set(rvalType.get),
              assignSemType,
              Some("Assignment and target types don't match"))
            Some(InternalPairSemType)
          } else {
            symbolTable.add(ident, assignSemType)
            Some(assignSemType)
          }
        }

      case assign@Assign(lvalue, rvalue) =>
        val opLvalSemType: Option[SemType] = checkLvalue(lvalue, symbolTable)

        opLvalSemType.get match {
          case FuncSemType(_, _, _) =>
            errorLog += TypeError(assign.pos,
              Set.empty,
              FuncSemType(InternalPairSemType, List.empty, 0),
              Some("Cannot assign to a function"))
            Some(InternalPairSemType)

          case lvalSemType: SemType =>
            // there are 5 rvalue cases
            rvalue match {
              case rval@ArrayLiter(_) =>
                val arrayType: Option[SemType] = checkArrayLiteral(rval, symbolTable)
                if (matchTypes(lvalSemType, arrayType.get)) {
                  return arrayType
                }
                errorLog += TypeError(assign.pos,
                  Set(ArraySemType(InternalPairSemType)),
                  lvalSemType,
                  Some("Can't assign array literal to non array type"))
                Some(InternalPairSemType)

              case rCall: Call =>
                val opFuncRetType: Option[SemType] = checkRvalue(rCall, symbolTable)
                if (matchTypes(opFuncRetType.get, lvalSemType)) {
                  return opFuncRetType
                }
                errorLog += TypeError(assign.pos,
                  Set(opFuncRetType.get),
                  lvalSemType,
                  Some("Function return type does not match target type"))
                Some(InternalPairSemType)

              case np@NewPair(expr1, expr2) =>
                val pt: PairSemType = lvalSemType match {
                  case p: PairSemType => p
                  case unexpectedType =>
                    errorLog += TypeError(np.pos,
                      Set(PairSemType(InternalPairSemType, InternalPairSemType)),
                      unexpectedType,
                      Some("Cannot assign a new pair to a non pair type"))
                    return None
                }
                val expr1Type = checkExpr(expr1, symbolTable)
                val expr2Type = checkExpr(expr2, symbolTable)

                if (!matchTypes(expr1Type.get, pt.pt1)) {
                  errorLog += TypeError(assign.pos,
                    Set(pt.pt1),
                    expr1Type.get,
                    Some("First of the pair doesn't match"))
                  return Some(InternalPairSemType)
                }
                if (!matchTypes(expr2Type.get, pt.pt2)) {
                  errorLog += TypeError(assign.pos,
                    Set(pt.pt2),
                    expr2Type.get,
                    Some("Second of the pair doesn't match"))
                  return Some(InternalPairSemType)
                }
                Some(pt)

              case Fst(rhsLVal) => checkPairElemAssign(rhsLVal, lvalSemType, symbolTable, is_fst = true)
              case Snd(rhsLVal) => checkPairElemAssign(rhsLVal, lvalSemType, symbolTable, is_fst = false)

              case expr: Expr =>
                val exprSemType: Option[SemType] = checkExpr(expr, symbolTable)
                if (matchTypes(exprSemType.get, lvalSemType)) {
                  return Some(lvalSemType)
                }
                errorLog += TypeError(assign.pos,
                  Set(exprSemType.get),
                  lvalSemType,
                  Some("Assignment and target types don't match"))
                Some(InternalPairSemType)

              case _ =>
                None
            }
        }

      case read@Read(lvalue) =>
        val readLvalSemType: Option[SemType] = checkLvalue(read.lvalue, symbolTable)
        val lvalPos = getLvalPos(lvalue)
        read.symbolTable = Some(symbolTable)
        readLvalSemType.get match {
          case IntSemType => Some(IntSemType)
          case CharSemType => Some(CharSemType)
          case InternalPairSemType =>
            errorLog += TypeErasureError((lvalPos._1._1, 1),
              Some("Cannot read into internal pair types due to type erasure"))
            Some(InternalPairSemType)
          case unexpectedType =>
            errorLog += new TypeError(lvalPos._1,
              Set(IntSemType, CharSemType),
              unexpectedType,
              Some("Can only read into int and char types"))(lvalPos._2)
            Some(InternalPairSemType)
        }

      case Free(expr: Expr) =>
        val exprType: Option[SemType] = checkExpr(expr, symbolTable)
        exprType.get match {
          case _: PairSemType => exprType
          case _: ArraySemType => exprType
          case unexpectedType =>
            val exprPos = getExprPos(expr)
            errorLog += new TypeError(exprPos._1,
              Set(PairSemType(InternalPairSemType, InternalPairSemType),
                ArraySemType(InternalPairSemType)),
              unexpectedType,
              Some("Can only free objects on heap"))(exprPos._2)
            Some(InternalPairSemType)
        }

      case ret@Return(expr: Expr) =>
        val funcRetType: Option[SemType] = symbolTable.lookupAll(ENCLOSING_FUNC_RETURN_TYPE)
        if (funcRetType.isDefined) {
          val exprType: Option[SemType] = checkExpr(expr, symbolTable)
          assert(exprType.isDefined)
          if (matchTypes(exprType.get, funcRetType.get)) {
            return exprType
          } else {

            val exprPos = getExprPos(expr)
            errorLog += new TypeError(exprPos._1,
              Set(funcRetType.get),
              exprType.get,
              Some("Return type does not match with function definition"))(exprPos._2)
            return Some(InternalPairSemType)
          }
        }
        errorLog += InvalidReturnError(ret.pos,
          Some("Main body cannot have return. Return must exist in a function body!"))
        Some(InternalPairSemType)

      case Exit(expr: Expr) =>
        val exprType: Option[SemType] = checkExpr(expr, symbolTable)
        exprType.get match {
          case IntSemType => exprType
          case _ =>
            val exprPos = getExprPos(expr)
            errorLog += new TypeError(exprPos._1,
              Set(IntSemType),
              exprType.get,
              Some("Cannot exit with a non integer exit code"))(exprPos._2)
            Some(InternalPairSemType)
        }

      case printStat@Print(expr: Expr) =>
        printStat.symbolTable = Some(symbolTable)
        checkExpr(expr, symbolTable)

      case printlnStat@Println(expr: Expr) =>
        printlnStat.symbolTable = Some(symbolTable)
        checkExpr(expr, symbolTable)

      case If(cond, thenStat, elseStat) =>
        val condType: Option[SemType] = checkExpr(cond, symbolTable)
        if (matchTypes(condType.get, BoolSemType)) {
          val thenScope = new SymbolTable(Some(symbolTable))
          val elseScope = new SymbolTable(Some(symbolTable))
          checkStatement(elseStat, elseScope)
          return checkStatement(thenStat, thenScope)
        }
        val condPos = getExprPos(cond)
        errorLog += TypeError(condPos._1, Set(BoolSemType), condType.get, Some("If expects a bool condition type"))
        Some(InternalPairSemType)

      case While(cond, doStat) =>
        val condType = checkExpr(cond, symbolTable)
        if (matchTypes(condType.get, BoolSemType)) {
          val doScope = new SymbolTable(Some(symbolTable))
          val statType = checkStatement(doStat, doScope)
          return statType
        }
        val condPos = getExprPos(cond)
        errorLog += TypeError(condPos._1, Set(BoolSemType), condType.get, Some("While expects a bool condition type"))
        Some(InternalPairSemType)

      case ScopeStat(stat) =>
        val newScope = new SymbolTable(Some(symbolTable))
        val statType = checkStatement(stat, newScope)
        statType

      case ConsecStat(first, next) =>
        checkStatement(first, symbolTable)
        val statType = checkStatement(next, symbolTable)
        statType
    }
  }

  private def checkParams(params: List[Param], symbolTable: SymbolTable, paramNames: mutable.Set[String]): Boolean = {
    for (param <- params) {
      if (paramNames.contains(param.ident)) {
        errorLog += DuplicateIdentifier(param.pos,
          param.ident,
          Some("Duplicate identifier found in function definition."))
        return false // duplicate func names
      } else {
        paramNames.add(param.ident)
        symbolTable.add(param.ident, convertToSem(param.paramType))
      }
    }
    true
  }

  private def isConcrete(pt1: SemType): Boolean = pt1 != InternalPairSemType

  private def matchInternalTypesAndConcreteCheck(rhsLval: LValue, pt : SemType, lvalSemType : SemType): Option[SemType] = {
    val lvalPos = getLvalPos(rhsLval)
    if (matchTypes(pt, lvalSemType)) {
      if (!isConcrete(lvalSemType) && !isConcrete(pt)) {
        errorLog += TypeErasureError((lvalPos._1._1, 1), Some("Both types need to be concrete"))
        return Some(InternalPairSemType)
      }
      else return Some(lvalSemType)
    }
    errorLog += new TypeError(lvalPos._1,
      Set(lvalSemType),
      pt,
      Some("Assignment and target types do not match"))(lvalPos._2)
    Some(InternalPairSemType)
  }

  private def checkPairElemAssign(rhsLval: LValue,
                                  lvalSemType: SemType,
                                  symbolTable: SymbolTable,
                                  is_fst: Boolean): Option[SemType] = {
    val rhsLvalType: Option[SemType] = checkLvalue(rhsLval, symbolTable)
    val lvalPos = getLvalPos(rhsLval)

    rhsLvalType.get match {
      case PairSemType(pt1, pt2) =>
        if (is_fst) {
          matchInternalTypesAndConcreteCheck(rhsLval,pt1, lvalSemType)
        } else {
          matchInternalTypesAndConcreteCheck(rhsLval,pt2, lvalSemType)
        }
      case InternalPairSemType =>
        if (!isConcrete(lvalSemType)) {
          errorLog += TypeErasureError((lvalPos._1._1, 1), Some("Type needs to be concrete"))
        }
        Some(InternalPairSemType)
      case unexpectedType =>
        errorLog += new TypeError(lvalPos._1,
          Set(PairSemType(InternalPairSemType, InternalPairSemType)), unexpectedType,
          Some("err fst applied to non pair type"))(lvalPos._2)
        Some(InternalPairSemType)
    }
  }

  private def checkArrayLiteral(arrayLit: ArrayLiter, symbolTable: SymbolTable): Option[SemType] = {
    if (arrayLit.exprs.nonEmpty) {
      val expType = checkExpr(arrayLit.exprs.head, symbolTable)
      for (expr <- arrayLit.exprs.tail) {
        val expTypeRest = checkExpr(expr, symbolTable)
        if (!matchTypes(expType.get, expTypeRest.get)) {
          val exprPos = getExprPos(expr)
          errorLog += new TypeError(exprPos._1,
            Set(expType.get),
            expTypeRest.get,
            Some("Arrays must contain elements of the same type"))(exprPos._2)
          return Some(InternalPairSemType)
        }
      }
      return Some(ArraySemType(expType.get))
    }
    Some(ArraySemType(InternalPairSemType))
  }
}