package wacc

import wacc.AST._
import wacc.ClassTable.{ClassDefinition, ClassTable}
import wacc.SemTypes._
import wacc.error._
import wacc.StructTable._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class semanticAnalyser {

  private final val WORD_SIZE = 4
  private final val BYTE_SIZE = 1

  // error semanticErrLogs for semantic analysis
  private final val errorLog = mutable.ListBuffer.empty[SemanticError]

  // classTable populated after semantic analysis of classes at the start of the program
  private var opClassTable: Option[ClassTable] = None

  // structTable populated after semantic analysis of structs
  private var opStructTable: Option[StructTable] = None

  // to store function return type in the intermediate scope
  private final val ENCLOSING_FUNC_RETURN_TYPE = "?_returnType"

  // mangling function names to maintain uniqueness
  private final val FUNCTION_PREFIX = "wacc_"

  // mangling class parameters and fields
  private final val CLASS_FIELD_PREFIX = "this."

  // mangling class method names
  private final val CLASS_METHOD_PREFIX = "wacc_class_"

  // flag set when checking class for method calls
  private var curClassName: Option[String] = None

  def checkProgram(program: Program, topLevelSymbolTable: SymbolTable[SemType]): Option[ListBuffer[SemanticError]] = {

    // absorbing structs next
    val structTable = new StructTable()
    val structDefinitions = mutable.Set.empty[Struct]
    for (struct <- program.structs) {
      if (structTable.lookup(struct.name).isEmpty) {
        structTable.add(struct.name)
        structDefinitions.add(struct)
      } else {
        errorLog += DuplicateIdentifier(struct.pos, struct.name, Some("Duplicate struct definition"))
      }
    }
    // set up struct for semantic/cg analysis
    opStructTable = Some(structTable)
    program.structTable = opStructTable

    // absorbing the classes from the top of the file
    val classTable = ClassTable()
    val classDefinitions = mutable.Set.empty[Class]
    for (waccClass <- program.classes) {
      if (classTable.lookup(waccClass.name).isEmpty) {
        classTable.add(waccClass.name)
        classDefinitions.add(waccClass)
      } else {
        errorLog += DuplicateIdentifier(waccClass.pos, waccClass.name, Some("Duplicate class definition"))
      }
    }
    // set up classes for semantic/cg analysis
    opClassTable = Some(classTable)
    program.classTable = opClassTable

    // semantics of classes before structs - order important for syntax
    for (waccClass <- classDefinitions) {
      checkClass(waccClass, classTable)
    }

    for (struct <- structDefinitions) {
      checkStruct(struct, structTable)
    }

    // first pass to develop function names on the top level symbol table
    val funcDefinitions = mutable.Set.empty[Func]
    for (func <- program.funcs) {
      if (topLevelSymbolTable.lookup(FUNCTION_PREFIX + func.ident).isEmpty) {
        topLevelSymbolTable.add(FUNCTION_PREFIX + func.ident, convertToSem(func))
        funcDefinitions.add(func)
      } else {
        errorLog += DuplicateIdentifier(func.pos, func.ident, Some("Duplicate function definition: " + func.ident))
      }
    }
    for (func <- funcDefinitions) {
      checkFunction(func, topLevelSymbolTable)
    }
    checkStatement(program.stat, topLevelSymbolTable, None)

    if (errorLog.nonEmpty) {
      return Some(errorLog)
    }
    None
  }

  private def getSize(assignType: SemType): Int = {
    assignType match {
      case SemTypes.CharSemType => BYTE_SIZE
      case _ => WORD_SIZE
    }
  }

  private def checkClass(waccClass: Class, classTable: ClassTable): Unit = {
    curClassName = Some(waccClass.name)

    val classDef: ClassDefinition = classTable.lookup(waccClass.name).get

    // To add and initialise the scope/type of parameters/fields and populate their respective tables
    val opOffset: Option[Int] = checkClassParams(waccClass.params, classDef, mutable.Set.empty[String])
    if (opOffset.isDefined) {
      var offset = opOffset.get
      for (field <- waccClass.fields) {
        val statSemType = checkStatement(field.varDec, classDef.getTypeTable, Some(CLASS_FIELD_PREFIX))
        if (statSemType.isDefined) {
          val name = field.varDec match {
            case vd@VarDec(_, ident, _) => {
              classDef.addField(vd)
              ident
            }
            case _ => throw new RuntimeException("Parsing should filter this")
          }

          if (classDef.getFieldScope(name).isDefined) {
            errorLog += DuplicateIdentifier(field.pos, name, Some("Duplicate field/parameter found in class definition"))
          } else {
            val size = statSemType.get match {
              case SemTypes.CharSemType => 1
              case _ => 4
            }
            classDef.addFieldOffsetScope(name, offset, field.scope)
            offset += size
          }
        }
      }
      classDef.setClassSize(offset)

      // now consume the class methods
      val methodDefinitions = collection.mutable.Set.empty[Method]
      for (method <- waccClass.methods) {
        if (classDef.getMethodScope(method.func.ident).isDefined) {
          errorLog += DuplicateIdentifier(method.func.pos, method.func.ident, Some("Duplicate identifier found in method definition"))
        } else {
          classDef.addMethodType(method.func.ident, convertToSem(method.func))
          classDef.addMethodOffsetScope(method.func.ident, method.scope)
          methodDefinitions.add(method)
        }
      }
      methodDefinitions.foreach(m => checkFunction(m.func, classDef.getTypeTable))
    }
    curClassName = None
  }

  private def checkClassParams(params: List[Param],
                               classDef: ClassDefinition,
                               paramNames: mutable.Set[String]): Option[Int] = {
    var offset = 0
    var err =  false
    for (param <- params) {
      if (paramNames.contains(param.ident)) {
        errorLog += DuplicateIdentifier(param.pos, param.ident, Some("Duplicate identifier found in class definition"))
        err = true
      } else {

        paramNames.add(param.ident)
        val paramType: SemType = convertToSem(param.paramType)
        val size = paramType match {
          case CharSemType => 1
          case _ => 4
        }
        classDef.addParam(param, offset, paramType)
        offset += size
      }
    }
    if (err) None else Some(offset)
  }

  private def checkStruct(struct: Struct, structTable: StructTable): Unit = {
    val structDef: StructDef = structTable.lookup(struct.name).get
    var currPointer: Int = 0
    val fieldNames = collection.mutable.Set.empty[String]
    for (fieldDec <- struct.fields) {
      // check validity of fieldDec Statement
      if (!fieldNames.contains(fieldDec.ident)) {
        val tType = convertToSem(fieldDec.assignType)
        val size = getSize(tType)
        structDef.add(fieldDec.ident, tType, currPointer)
        currPointer += size
      } else {
        errorLog += DuplicateIdentifier(fieldDec.pos, fieldDec.ident, Some("Duplicate field found in struct " + struct.name))
      }
    }
    structDef.structSize = currPointer
  }

  private def getLvalPos(lval: LValue): ((Int, Int), Int) = {
    lval match {
      case ident: IdentValue => (ident.pos, 0)
      case arrayElem: ArrayElem => (arrayElem.pos, 0)
      case structElem: StructElem => (structElem.pos, 0)
      case classElem: ClassElem => (classElem.pos, 0)
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
      case VoidType() => VoidSemType
      case IntType() => IntSemType
      case BoolType() => BoolSemType
      case CharType() => CharSemType
      case StringType() => StringSemType
      case ArrayType(t: Type) => ArraySemType(convertToSem(t))
      case PairType(pt1, pt2) => PairSemType(convertToSem(pt1), convertToSem(pt2))
      case StructType(structName) => StructSemType(structName)
      case ClassType(className) => ClassSemType(className)
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
      case PairType(pt1, pt2) => PairSemType(convertToSem(pt1), convertToSem(pt2))
      case StructType(structName) => StructSemType(structName)
      case ClassType(className) => ClassSemType(className)
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

      case ClassSemType(className1) => {
        type2 match {
          case ClassSemType(className2) => className1 == className2
          case InternalPairSemType => true
          case _ => false
        }
      }

      case StructSemType(ident1) => {
        type2 match {
          case StructSemType(ident2) => {
            if (ident1 equals ident2) true
            else {
              val structTable = opStructTable.get
              val structDef1 = structTable.lookup(ident1).get
              val structDef2 = structTable.lookup(ident1).get
              val fields1 = structDef1.getKeys()
              val fields2 = structDef2.getKeys()
              var out = true
              for (i <- fields1.indices) {
                if (!matchTypes(structDef1.lookup(fields1(i)).get, structDef2.lookup(fields2(i)).get)) {
                  out = false
                }
              }
              out
            }
          }
          case InternalPairSemType => true
          case _ => false
        }
      }
      case IfExprSemType(cond, thenExpr, elseExpr) => matchTypes(cond, BoolSemType) && matchTypes(thenExpr, type2) && matchTypes(elseExpr, type2)
      case _ => matchBaseTypes(type1, type2)
    }
  }

  private def checkExpr(expr: Expr, symbolTable: GenericTable[SemType]): Option[SemType] = {
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
      case ex@ThisExpr(ident) =>
        val thisIdent = CLASS_FIELD_PREFIX + ident
        val identType = symbolTable.lookupAll(thisIdent)
        if (identType.isDefined) {
          ex.st = Some(symbolTable)
          return identType
        }
        errorLog += UnknownIdentifierError(ex.pos, thisIdent, Some("Unknown class variable identifier found"))
        Some(InternalPairSemType)
      case PairExpr() => Some(InternalPairSemType)
      case arrayElem: ArrayElem =>
        arrayElem.st = Some(symbolTable)
        checkArrayElem(arrayElem, symbolTable)

      case structElem: StructElem =>
        structElem.st = Some(symbolTable)
        checkStructElem(structElem, symbolTable)

      case classElem: ClassElem =>
        classElem.st = Some(symbolTable)
        checkClassElem(classElem, symbolTable)

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
      case ifExpression@IfExpr(cond, thenExpr, elseExpr) => {
        val condition: Option[SemType] = checkExpr(cond, symbolTable)
        if (matchTypes(condition.get, BoolSemType)) {
          val thenScope = new SymbolTable(Some(symbolTable))
          val elseScope = new SymbolTable(Some(symbolTable))
          val thenCond: Option[SemType] = checkExpr(thenExpr, thenScope)
          val elseCond: Option[SemType] = checkExpr(elseExpr, elseScope)
          assert(thenCond.isDefined, "thenCond should be defined")
          assert(elseCond.isDefined, "elseCond should be defined")
          if (matchTypes(elseCond.get, thenCond.get)) {
            return elseCond
          }
          else {
            val condPos = getExprPos(ifExpression)
            errorLog += TypeError(condPos._1, Set(thenCond.get), elseCond.get, Some("All branches of the if starting here should have same return types"))
            return Some(InternalPairSemType)
          }
        }
        val condPos = getExprPos(cond)
        errorLog += TypeError(condPos._1, Set(BoolSemType), condition.get, Some("If expects a bool condition type"))
        Some(InternalPairSemType)
      }

      case _ => System.err.println("Should not reach here")
        Some(InternalPairSemType)
    }
  }

  private def checkSameType(e1: Expr, e2: Expr, symbolTable: GenericTable[SemType]): Option[SemType] = {
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

  private def checkComparisonBinOp(e1: Expr, e2: Expr, symbolTable: GenericTable[SemType]): Option[SemType] = {
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
                                 symbolTable: GenericTable[SemType], matchBaseType: SemType): Option[SemType] = {
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
      case thisExpr: ThisExpr => (thisExpr.pos, 0)
      case arrayElem: ArrayElem => (arrayElem.pos, 0)
      case ifExpr: IfExpr => (ifExpr.pos, 0)
      case structElem: StructElem => (structElem.pos, 0)
      case classElem: ClassElem => (classElem.pos, 0)

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

  def checkStructLiteral(structLiter: StructLiter, symbolTable: GenericTable[SemType]): Option[SemType] = {
    val structTable = opStructTable.get

    assert(structLiter.exprs.nonEmpty, "Empty struct literal not allowed!")
    val exprTypes = ListBuffer.empty[SemType]
    structLiter.exprs.foreach(expr => exprTypes += checkExpr(expr, symbolTable).get)
    var foundName: Option[String] = None
    val keys = structTable.getKeys().toList
    var i = 0

    while (foundName.isEmpty && i < keys.length) {
      val structDef: StructDef = structTable.lookup(keys(i)).get
      val fields = structDef.getKeys()
      var j = 0
      var matches = true
      val bound1 = exprTypes.length
      val bound2 = fields.length

      while (matches && j < bound1 && j < bound2) {
        if (!matchTypes(structDef.lookup(fields(j)).get, exprTypes(j))) matches = false
        j += 1

        if (j >= bound1 && j < bound2) matches = false
        if (j < bound1 && j >= bound2) matches = false
      }

      if (matches) foundName = Some(keys(i))
      i += 1
    }

    if (foundName.isDefined) {
      Some(StructSemType(foundName.get))
    } else {
      errorLog += UnknownObjectError(getExprPos(structLiter.exprs.head)._1,
        Some("Struct assignment doesn't match any existing struct type"))

      Some(InternalPairSemType)
    }
  }


  def checkNewClass(newClass: NewClass, symbolTable: GenericTable[SemType]): Option[SemType] = {
    val classTable: ClassTable = opClassTable.get

    val opClassDefinition = classTable.lookup(newClass.className)

    if (opClassDefinition.isDefined) {
      val classDefn: ClassDefinition = opClassDefinition.get

      // checking that parameter lengths match
      var err = false
      if (newClass.exprs.size == classDefn.numParams) {

        for (i <- newClass.exprs.indices) {
          val expType = checkExpr(newClass.exprs(i), symbolTable)
          val paramType = convertToSem(classDefn.getParams(i).paramType)
          if (!matchTypes(expType.get, paramType)) {
            val exprPos = getExprPos(newClass.exprs(i))
            errorLog += new TypeError(exprPos._1,
              Set(paramType), expType.get,
              Some("Argument type does not match with parameter"))(exprPos._2)
            err = true
          }
        }
        if (err) Some(InternalPairSemType) else Some(ClassSemType(newClass.className))
      } else {
        errorLog += ArityMismatch(newClass.pos, classDefn.numParams, newClass.exprs.size, Some("Wrong number of class parameters"))
        Some(InternalPairSemType)
      }
    } else {
      errorLog += UnknownIdentifierError(newClass.pos, newClass.className, Some("Unknown class name found"))
      Some(InternalPairSemType)
    }
  }


  private def checkRvalue(rvalue: RValue, symbolTable: GenericTable[SemType]): Option[SemType] = {
    rvalue match {
      case expr: Expr => checkExpr(expr, symbolTable)

      case arrayLiter: ArrayLiter => checkArrayLiteral(arrayLiter, symbolTable)

      case structLiter: StructLiter => checkStructLiteral(structLiter, symbolTable)

      case nc: NewClass => checkNewClass(nc, symbolTable)

      case NewPair(e1: Expr, e2: Expr) =>
        val e1Type: Option[SemType] = checkExpr(e1, symbolTable)
        val e2Type: Option[SemType] = checkExpr(e2, symbolTable)
        Some(PairSemType(e1Type.get, e2Type.get))


      case mc@MethodCall(ident, methodName, args) => {
        val opType = symbolTable.lookupAll(ident)
        if (opType.isEmpty) {
          errorLog += UnknownIdentifierError(mc.pos, ident, Some("Unknown identifier found "))
          return Some(InternalPairSemType)
        }

        opType.get match {
          case ClassSemType(className) => {
            val opClassDEfn = opClassTable.get.lookup(className)
            if (opClassDEfn.isDefined) {
              val classDefinition: ClassDefinition = opClassDEfn.get
              //              val mangledMethodName = FUNCTION_PREFIX + methodName
              val opType = classDefinition.getMethodType(methodName)

              if (opType.isDefined) {
                opType.get match {
                  case FuncSemType(retType, paramTypes, numParams) => {
                    val opScope: Option[Scope] = classDefinition.getMethodScope(methodName)

                    // Checking if method is callable
                    opScope.get match {
                      case Public() =>
                        // parameters length match
                        if (numParams != args.length) {
                          errorLog += ArityMismatch(mc.pos, numParams, args.length,
                            Some("Wrong number of method arguments"))
                          return Some(InternalPairSemType)
                        }
                        var err  = false
                        // parameters and arguments type match
                        for (i <- args.indices) {
                          val expType = checkExpr(args(i), symbolTable)
                          if (!matchTypes(expType.get, paramTypes(i))) {
                            val argPos = getExprPos(args(i))
                            errorLog += new TypeError(argPos._1,
                              Set(paramTypes(i)), expType.get,
                              Some("Argument type does not match with parameter"))(argPos._2)
                            err = true
                          }
                        }
                        mc.symbolTable = Some(symbolTable)
                        if (err) Some(InternalPairSemType) else Some(retType)

                      case Private() =>
                        errorLog +=
                          InvalidScopeError(mc.pos, methodName, Some("Cannot access private members " + methodName))
                        Some(InternalPairSemType)
                    }

                  }
                  case otherType => {
                    errorLog += new TypeError(mc.pos,
                      Set(FuncSemType(InternalPairSemType, List.empty, 0)), otherType,
                      Some("Cannot call non methods"))(0)
                    Some(InternalPairSemType)
                  }
                }
              } else {
                errorLog += UnknownIdentifierError(mc.pos, methodName, Some("Unknown method identifier found"))
                Some(InternalPairSemType)
              }
            } else {
              errorLog += UnknownObjectError(mc.pos,
                Some("No such class is defined"))
              Some(InternalPairSemType)
            }
          }
          case otherType => errorLog += new TypeError(mc.pos,
            Set(ClassSemType("")), otherType,
            Some("Method call can only be done on classes"))(0)
            Some(InternalPairSemType)
        }
      }

      case call@Call(ident, args) => {

        // valid function in symbol table
        var identSemType: Option[SemType] = None
        if (curClassName.isDefined) {
          identSemType = symbolTable.lookupAll(CLASS_METHOD_PREFIX + curClassName.get + "_" + ident)
        } else {
          identSemType = symbolTable.lookupAll(FUNCTION_PREFIX + ident)
        }

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
            var err = false
            for (i <- args.indices) {
              val expType = checkExpr(args(i), symbolTable)
              if (!matchTypes(expType.get, funcType.paramTypes(i))) {
                val argPos = getExprPos(call.args(i))
                errorLog += new TypeError(argPos._1,
                  Set(funcType.paramTypes(i)), expType.get,
                  Some("Argument type does not match with parameter"))(argPos._2)
                err = true
              }
            }
            if (err) {
              Some(InternalPairSemType)
            } else {
              Some(funcType.retType)
            }

          case unexpectedType =>
            errorLog += TypeError(call.pos,
              Set(FuncSemType(InternalPairSemType, List.empty, 0)), unexpectedType,
              Some("Cannot call a non function identifier"))
            Some(InternalPairSemType)
        }
      }

      case elem: PairElem => checkPairElem(elem, symbolTable)
    }
  }

  private def checkLvalue(lvalue: LValue, symbolTable: GenericTable[SemType]): Option[SemType] = {
    lvalue match {
      case ident@IdentValue(name: String) =>
        val identType = symbolTable.lookupAll(name)
        if (identType.isDefined) {
          ident.st = Some(symbolTable)
          return identType
        }
        errorLog += UnknownIdentifierError(ident.pos, ident.s, Some("Unknown variable Identifier found"))
        Some(InternalPairSemType)
      case arrayElem: ArrayElem => checkArrayElem(arrayElem, symbolTable)
      case structElem: StructElem => checkStructElem(structElem, symbolTable)
      case classElem: ClassElem => checkClassElem(classElem, symbolTable)
      case elem: PairElem => checkPairElem(elem, symbolTable)
    }
  }

  private def checkClassElem(classElem: ClassElem, symbolTable: GenericTable[SemType]): Option[SemType] = {
    val identType: Option[SemType] = symbolTable.lookupAll(classElem.ident)
    if (identType.isDefined) {
      identType.get match {

        case ClassSemType(className) => {
          val opClassDefn: Option[ClassDefinition] = opClassTable.get.lookup(className)
          if (opClassDefn.isDefined) {
            val opScope: Option[Scope] = opClassDefn.get.getFieldScope(classElem.member)
            if (opScope.isDefined) {
              opScope.get match {
                case Public() =>
                  classElem.st = Some(symbolTable)
                  classElem.className = Some(className)
                  return opClassDefn.get.getFieldType(classElem.member)

                case Private() =>
                  errorLog +=
                    InvalidScopeError(classElem.pos, classElem.member, Some("Cannot access private members " + classElem.member))
                  return Some(InternalPairSemType)
              }
            } else {
              errorLog +=
                UnknownIdentifierError(classElem.pos, classElem.member,
                  Some("Cannot find member inside the class " + classElem.ident))
              return Some(InternalPairSemType)
            }
          } else {
            return Some(InternalPairSemType)
          }
        }

        case unexpectedType => {
          errorLog += TypeError(classElem.pos,
            Set(ClassSemType("")),
            unexpectedType,
            Some("Expected a struct type for accessing a field"))
          return Some(InternalPairSemType)
        }
      }
    }

    errorLog += UnknownIdentifierError(classElem.pos, classElem.ident, Some("Unknown class identifier found "))
    Some(InternalPairSemType)
  }

  private def checkStructElem(structElem: StructElem, symbolTable: GenericTable[SemType]): Option[SemType] = {
    val identType: Option[SemType] = symbolTable.lookupAll(structElem.ident)
    if (identType.isDefined) {
      identType.get match {
        case StructSemType(ident) => {
          val structDef: Option[StructDef] = opStructTable.get.lookup(ident)
          if (structDef.isDefined) {
            val opType: Option[SemType] = structDef.get.lookup(structElem.field)
            if (opType.isDefined) {
              structElem.st = Some(symbolTable)
              return opType
            } else {
              errorLog +=
                UnknownIdentifierError(structElem.pos, structElem.field,
                  Some("Cannot find field inside the struct " + structElem.ident))
              return Some(InternalPairSemType)
            }
          } else {
            errorLog += UnknownObjectError(structElem.pos,
              Some("Cannot find struct type for struct elem access"))
            return Some(InternalPairSemType)
          }
        }
        case unexpectedType => {
          errorLog += TypeError(structElem.pos,
            Set(StructSemType("")),
            unexpectedType,
            Some("Expected a struct type for accessing a field"))
          return Some(InternalPairSemType)
        }
      }
    }

    errorLog += UnknownIdentifierError(structElem.pos, structElem.ident, Some("Unknown struct identifier found "))
    Some(InternalPairSemType)
  }

  private def checkArrayElem(arrayElem: ArrayElem, symbolTable: GenericTable[SemType]): Option[SemType] = {
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
      arrayElem.st = Some(symbolTable)
      return Some(arrayTypeHolder)
    }
    errorLog += UnknownIdentifierError(arrayElem.pos, arrayElem.ident, Some("Unknown array identifier found"))
    Some(InternalPairSemType)
  }

  def attachType(pe: PairElem, ty: SemType): Unit = {
    pe match {
      case f: Fst => f.ty = ty
      case s: Snd => s.ty = ty
    }
  }

  private def checkPairElem(pe: PairElem, symbolTable: GenericTable[SemType]): Option[SemType] = {
    var is_fst: Boolean = false
    val insideLval: LValue = pe match {
      case Fst(lvalue) =>
        is_fst = true
        lvalue
      case Snd(lvalue) => lvalue
    }

    insideLval match {
      case insideElem: PairElem => {
        val insidePos = insideElem match {
          case f: Fst => f.pos
          case s: Snd => s.pos
        }
        val insideType = checkPairElem(insideElem, symbolTable)
        assert(insideType.isDefined, "check pair elem on inside type failed")
        insideType.get match {
          case PairSemType(pt1, pt2) => {
            if (is_fst) {
              attachType(pe, pt1)
              Some(pt1)
            } else {
              attachType(pe, pt2)
              Some(pt2)
            }
          }
          case unexpectedType => {
            errorLog += TypeError(insidePos,
              Set(PairSemType(InternalPairSemType, InternalPairSemType)), unexpectedType,
              Some("can only call fst or snd on pairs"))
            Some(InternalPairSemType)
          }
        }
      }

      case classElem: ClassElem => {
        val opClassElemType = checkClassElem(classElem, symbolTable)
        opClassElemType.get match {
          case PairSemType(pt1, pt2) =>
            if (is_fst) {
              attachType(pe, pt1)
              Some(pt1)
            } else {
              attachType(pe, pt2)
              Some(pt2)
            }
          case InternalPairSemType => Some(InternalPairSemType)
          case unexpectedType =>
            errorLog += TypeError(classElem.pos,
              Set(PairSemType(InternalPairSemType, InternalPairSemType)), unexpectedType,
              Some("can only call fst or snd on pairs"))
            Some(InternalPairSemType)
        }
      }

      case arrayElem: ArrayElem =>
        val opArrayItemType = checkArrayElem(arrayElem, symbolTable)
        opArrayItemType.get match {
          case PairSemType(pt1, pt2) =>
            if (is_fst) {
              attachType(pe, pt1)
              Some(pt1)
            } else {
              attachType(pe, pt2)
              Some(pt2)
            }
          case InternalPairSemType => Some(InternalPairSemType)
          case unexpectedType =>
            errorLog += TypeError(arrayElem.pos,
              Set(PairSemType(InternalPairSemType, InternalPairSemType)), unexpectedType,
              Some("can only call fst or snd on pairs"))
            Some(InternalPairSemType)
        }

      case structElem: StructElem =>
        val opStructElemType = checkStructElem(structElem, symbolTable)
        opStructElemType.get match {
          case PairSemType(pt1, pt2) =>
            if (is_fst) {
              attachType(pe, pt1)
              Some(pt1)
            } else {
              attachType(pe, pt2)
              Some(pt2)
            }
          case InternalPairSemType => Some(InternalPairSemType)
          case unexpectedType =>
            errorLog += TypeError(structElem.pos,
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
                attachType(pe, pt1)
                ident.st = Some(symbolTable)
                return Some(pt1)
              } else {
                attachType(pe, pt2)
                ident.st = Some(symbolTable)
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

  private def checkFunction(func: Func, symbolTable: SymbolTable[SemType]): Option[SemType] = {
    val intermediateTable = new SymbolTable(Some(symbolTable))
    if (checkParams(func.params, intermediateTable, mutable.Set.empty[String])) {
      val funcSemType: SemType = convertToSem(func.retType)
      intermediateTable.add(ENCLOSING_FUNC_RETURN_TYPE, funcSemType)
      val childSym = new SymbolTable(Some(intermediateTable))
      if (checkStatement(func.stat, childSym, None).isDefined) {
        func.st = Some(childSym)
        return Some(funcSemType)
      }

    }
    None
  }

  private def checkStatement(node: Statement, symbolTable: GenericTable[SemType], prefix: Option[String]): Option[SemType] = {
    var err = false
    node match {
      case Skip => Some(InternalPairSemType)
      case MatchStat(cond, condStatList) =>{
        val condType: Option[SemType] = checkExpr(cond, symbolTable)
        for (condStat <- condStatList) {
          checkStatement(condStat._2, symbolTable, None)
          val condStatType: Option[SemType] = checkExpr(condStat._1, symbolTable)
          if (!matchTypes(condType.get, condStatType.get)) {
            val condPos = getExprPos(condStat._1)
            errorLog += TypeError(condPos._1, Set(condStatType.get), condType.get, Some("Expression Type doesn't match type of match expression"))
            err = true
          }
        }
        if (err) {
          Some(InternalPairSemType)
        } else {
          condType
        }
      }
      case varDec@VarDec(assignType, ident, rvalue) =>

        if (symbolTable.lookup(ident).isDefined) {
          errorLog += DuplicateIdentifier(varDec.pos, varDec.ident, Some("Duplicate variable identifier found"))
          Some(InternalPairSemType)
        } else {
          val rvalType: Option[SemType] = checkRvalue(rvalue, symbolTable)
          val assignSemType = convertToSem(assignType)

          val notFound: Boolean = assignSemType match {
            case StructSemType(ident) => opStructTable.get.lookup(ident).isEmpty
            case ClassSemType(className) => opClassTable.get.lookup(className).isEmpty
            case _ => false
          }

          if (notFound) {
            errorLog += UnknownIdentifierError(varDec.pos, varDec.ident, Some("Unknown object type found: " + ident))
            return Some(InternalPairSemType)
          }

          if (!matchTypes(assignSemType, rvalType.get)) {
            errorLog += TypeError(varDec.pos,
              Set(rvalType.get),
              assignSemType,
              Some("Assignment and target types don't match"))
            Some(InternalPairSemType)
          } else {
            symbolTable.add(prefix.getOrElse("") + ident, assignSemType)
            varDec.symbolTable = Some(symbolTable)
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
          case _: StructSemType => exprType
          case _: ClassSemType => exprType
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

      case mc@MethodStat(ident, methodName, args) => {
        val opType = symbolTable.lookupAll(ident)
        if (opType.isEmpty) {
          errorLog += UnknownIdentifierError(mc.pos, ident, Some("Unknown identifier found "))
          return Some(InternalPairSemType)
        }

        opType.get match {
          case ClassSemType(className) => {
            val opClassDefinition = opClassTable.get.lookup(className)
            if (opClassDefinition.isDefined) {
              val classDefinition: ClassDefinition = opClassDefinition.get
              //              val mangledMethodNames = FUNCTION_PREFIX + methodName
              val opType = classDefinition.getMethodType(methodName)

              if (opType.isDefined) {
                opType.get match {
                  case FuncSemType(retType, paramTypes, numParams) => {
                    val opScope: Option[Scope] = classDefinition.getMethodScope(methodName)

                    // Checking if method is callable
                    opScope.get match {
                      case Public() =>
                        // parameters length match
                        if (numParams != args.length) {
                          errorLog += ArityMismatch(mc.pos, numParams, args.length,
                            Some("Wrong number of method arguments"))
                          return Some(InternalPairSemType)
                        }

                        var err = false
                        // parameters and arguments type match
                        for (i <- args.indices) {
                          val expType = checkExpr(args(i), symbolTable)
                          if (!matchTypes(expType.get, paramTypes(i))) {
                            val argPos = getExprPos(args(i))
                            errorLog += new TypeError(argPos._1,
                              Set(paramTypes(i)), expType.get,
                              Some("Argument type does not match with parameter"))(argPos._2)
                            err = true

                          }
                        }
                        mc.st = Some(symbolTable)
                        if (err) Some(InternalPairSemType) else Some(retType)


                      case Private() =>
                        errorLog +=
                          InvalidScopeError(mc.pos, methodName, Some("Cannot access private members " + methodName))
                        Some(InternalPairSemType)
                    }

                  }
                  case otherType => {
                    errorLog += new TypeError(mc.pos,
                      Set(FuncSemType(InternalPairSemType, List.empty, 0)), otherType,
                      Some("Cannot call non methods"))(0)
                    Some(InternalPairSemType)
                  }
                }
              } else {
                errorLog += UnknownIdentifierError(mc.pos, methodName, Some("Unknown method identifier found"))
                Some(InternalPairSemType)
              }
            } else {
              errorLog += UnknownObjectError(mc.pos,
                Some("No such class is defined"))
              Some(InternalPairSemType)
            }
          }
          case otherType => errorLog += new TypeError(mc.pos,
            Set(ClassSemType("")), otherType,
            Some("Method call can only be done on classes"))(0)
            Some(InternalPairSemType)
        }
      }


      case printStat: Print =>
        printStat.symbolTable = Some(symbolTable)
        printStat.expType = checkExpr(printStat.e, symbolTable)
        printStat.expType

      case printlnStat: Println =>
        printlnStat.symbolTable = Some(symbolTable)
        printlnStat.expType = checkExpr(printlnStat.e, symbolTable)
        printlnStat.expType

      case If(cond, thenStat, elseStat) =>
        val condType: Option[SemType] = checkExpr(cond, symbolTable)
        if (matchTypes(condType.get, BoolSemType)) {
          val thenScope = new SymbolTable(Some(symbolTable))
          val elseScope = new SymbolTable(Some(symbolTable))
          checkStatement(elseStat, elseScope, prefix)
          return checkStatement(thenStat, thenScope, prefix)
        }
        val condPos = getExprPos(cond)
        errorLog += TypeError(condPos._1, Set(BoolSemType), condType.get, Some("If expects a bool condition type"))
        Some(InternalPairSemType)

      case IfThen(cond, thenStat) =>
        val condType: Option[SemType] = checkExpr(cond, symbolTable)
        if (matchTypes(condType.get, BoolSemType)) {
          val thenScope = new SymbolTable(Some(symbolTable))
          return checkStatement(thenStat, thenScope, None)
        }
        val condPos = getExprPos(cond)
        errorLog += TypeError(condPos._1, Set(BoolSemType), condType.get, Some("If expects a bool condition type"))
        Some(InternalPairSemType)

      case While(cond, doStat) =>
        val condType = checkExpr(cond, symbolTable)
        if (matchTypes(condType.get, BoolSemType)) {
          val doScope = new SymbolTable(Some(symbolTable))
          val statType = checkStatement(doStat, doScope, prefix)
          return statType
        }
        val condPos = getExprPos(cond)
        errorLog += TypeError(condPos._1, Set(BoolSemType), condType.get, Some("While expects a bool condition type"))
        Some(InternalPairSemType)

      case ScopeStat(stat) =>
        val newScope = new SymbolTable(Some(symbolTable))
        val statType = checkStatement(stat, newScope, prefix)
        statType

      case ConsecStat(first, next) =>
        checkStatement(first, symbolTable, prefix)
        val statType = checkStatement(next, symbolTable, prefix)
        statType

      case cs@CallStat(ident, args) => {
        // valid function in symbol table
        val identSemType = symbolTable.lookupAll(FUNCTION_PREFIX + ident)
        if (identSemType.isEmpty) {
          errorLog += UnknownIdentifierError(cs.pos, ident, Some("Unknown function identifier found"))
          return Some(InternalPairSemType)
        }

        identSemType.get match {
          case funcType: FuncSemType =>
            // parameters length match
            if (funcType.numParams != args.length) {
              errorLog += ArityMismatch(cs.pos,
                funcType.numParams,
                args.length,
                Some("Wrong number of function arguments"))
              return Some(InternalPairSemType)
            }

            // parameters and arguments type match
            var err = false
            for (i <- args.indices) {
              val expType = checkExpr(args(i), symbolTable)
              if (!matchTypes(expType.get, funcType.paramTypes(i))) {
                val argPos = getExprPos(cs.args(i))
                errorLog += new TypeError(argPos._1,
                  Set(funcType.paramTypes(i)), expType.get,
                  Some("Argument type does not match with parameter"))(argPos._2)
                err = true
              }
            }
            if (err) Some(InternalPairSemType) else Some(funcType.retType)

          case unexpectedType =>
            errorLog += TypeError(cs.pos,
              Set(FuncSemType(InternalPairSemType, List.empty, 0)), unexpectedType,
              Some("Cannot call a non function identifier"))
            Some(InternalPairSemType)
        }
      }
    }
  }

  private def checkParams(params: List[Param], symbolTable: SymbolTable[SemType], paramNames: mutable.Set[String]): Boolean = {
    var out = true
    for (param <- params) {
      if (paramNames.contains(param.ident)) {
        errorLog += DuplicateIdentifier(param.pos,
          param.ident,
          Some("Duplicate identifier found in function definition."))
        out = false // duplicate func names
      } else {
        paramNames.add(param.ident)
        symbolTable.add(param.ident, convertToSem(param.paramType))
      }
    }
    out
  }

  private def isConcrete(pt1: SemType): Boolean = pt1 != InternalPairSemType

  private def matchInternalTypesAndConcreteCheck(rhsLval: LValue, pt: SemType, lvalSemType: SemType): Option[SemType] = {
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
                                  symbolTable: GenericTable[SemType],
                                  is_fst: Boolean): Option[SemType] = {
    val rhsLvalType: Option[SemType] = checkLvalue(rhsLval, symbolTable)
    val lvalPos = getLvalPos(rhsLval)

    rhsLvalType.get match {
      case PairSemType(pt1, pt2) =>
        if (is_fst) {
          matchInternalTypesAndConcreteCheck(rhsLval, pt1, lvalSemType)
        } else {
          matchInternalTypesAndConcreteCheck(rhsLval, pt2, lvalSemType)
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

  private def checkArrayLiteral(arrayLit: ArrayLiter, symbolTable: GenericTable[SemType]): Option[SemType] = {
    if (arrayLit.exprs.nonEmpty) {
      var err = false
      val expType = checkExpr(arrayLit.exprs.head, symbolTable)
      for (expr <- arrayLit.exprs.tail) {
        val expTypeRest = checkExpr(expr, symbolTable)
        if (!matchTypes(expType.get, expTypeRest.get)) {
          val exprPos = getExprPos(expr)
          errorLog += new TypeError(exprPos._1,
            Set(expType.get),
            expTypeRest.get,
            Some("Arrays must contain elements of the same type"))(exprPos._2)
          err = true
        }
      }
      if (err) Some(InternalPairSemType) else Some(ArraySemType(expType.get))
    } else {
      Some(ArraySemType(InternalPairSemType))
    }
  }
}