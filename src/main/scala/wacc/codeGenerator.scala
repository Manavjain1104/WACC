package wacc

import wacc.AST._
import wacc.IR._
import wacc.Registers._
import wacc.SemTypes._

import scala.collection.mutable.ListBuffer

class codeGenerator(program: Program) {

  private val paramRegs = List(R0, R1, R2, R3)
  private var labelOrder = 0
  private val FUNCTION_PREFIX = "wacc_"
  private val WORDSIZE = 4

  private val strings: ListBuffer[String] = ListBuffer.empty[String]
  private var stringNum = 0

  private val widgets = collection.mutable.Map.empty[String, collection.mutable.Set[String]]

  /*
  TODO - Back End
  1) Free
  2) Heaps - pair
  3) Overflow, Div by 0, IntExpr large Int
  4) Squish to 1 byte char arr, print arr addr
   */

  def generateProgIR(): List[IR] = {
    val irs = ListBuffer.empty[IR]

    // assembly hygiene
    irs += Global(List("main"))

    // this is consuming the *Main* body of the program
    val localRegs: List[Reg] = List(R4, R5, R6, R7, R0, R1, R2, R3)
    val numLocalRegs = localRegs.length

    irs += Label("main")
    irs += PUSHMul(List(FP, LR))
    irs.append(MOV(FP, SP, "Default"))
    val numLocalsInMain = findNumLocals(program.stat)
    if (numLocalsInMain > numLocalRegs) {
      irs.append(SUB(SP, SP, (numLocalsInMain - numLocalRegs) * WORDSIZE))
    }
    val liveMap = new SymbolTable[Location](None)
    irs.appendAll(generateStatIR(program.stat, liveMap, localRegs, 0))

    if (numLocalsInMain > numLocalRegs) {
      irs.append(ADD(SP, SP, (numLocalsInMain - numLocalRegs) * WORDSIZE, "Default"))
    }
    irs += MOVImm(R0, 0, "Default")
    irs += POPMul(List(FP, PC))

    // generate assembly for program functions
    for (func <- program.funcs) {
      irs.appendAll(generateFuncIR(func, localRegs))
    }
    irs.prepend(Data(strings.toList, 0))

    // checking widget
    if (widgets.contains("print")) {
      widgets("print").foreach(flag => irs.appendAll(printAll(flag)))
    }
    if (widgets.contains("read")) {
      widgets("read").foreach(flag => irs.appendAll(readAll(flag)))
    }
    if (widgets.contains("printNewLine")) irs.appendAll(printNewLine())
    if (widgets.contains("arrLoad")) irs.appendAll(arrLoad())
    if (widgets.contains("arrStore")) irs.appendAll(arrStore())
    if (widgets.contains("arrLoadB")) irs.appendAll(arrLoadB())
    if (widgets.contains("arrStoreB")) irs.appendAll(arrStoreB())
    if (widgets.contains("boundsCheck")) {
      irs.appendAll(boundsCheck())
    }

    optimisePushPop(irs.toList)
//    irs.toList
  }

  // this function writes instructions that calculate the value of the expression
  // and leave them on top of the stack, only use R8 and R9 as temporaries
  def generateExprIR(expr: Expr, liveMap : SymbolTable[Location]): List[IR] = {
    val locals = liveMap.getNestedEntries()
    expr match {
      case IntExpr(x) => List(MOVImm(scratchReg1, x, "Default"), PUSH(scratchReg1))
      case BoolExpr(b) => {
        var value = 0
        if (b) value = 1
        List(MOVImm(scratchReg1, value, "Default"), PUSH(scratchReg1))
      }
      case CharExpr(c) => {
        List(MOVImm(scratchReg1, c.toInt, "Default"), PUSH(scratchReg1))
      }
      case StringExpr(s) => {
        val found = strings.find(st => s.equals(st))
        var num = 0
        if (found.isEmpty) {
          num = stringNum
          stringNum += 1
          strings += s
        } else num = strings.indexOf(s)
        List(StringInit(scratchReg1, num), PUSH(scratchReg1))
      }

      case IdentExpr(ident) => {
        // transfer identifier expr to the first available register
        getIntoTarget(ident, scratchReg1, liveMap).appended(PUSH(scratchReg1))
      }

      case expr: UnopExpr => {
        expr match {
          case ChrExpr(e) => generateExprIR(e, liveMap)
          case OrdExpr(e) => generateExprIR(e, liveMap)
          case NegExpr(e) => generateExprIR(e, liveMap) ++ List(POP(scratchReg1), NEG(scratchReg1, scratchReg1), PUSH(scratchReg1))
          case NotExpr(e) => generateExprIR(e, liveMap) ++ List(POP(scratchReg1), NOT(scratchReg1, scratchReg1), PUSH(scratchReg1))
          case LenExpr(e) => {
            val irs = ListBuffer.empty[IR]
            e match {
               case IdentExpr(ident) => {
                 irs.appendAll(getIntoTarget(ident, scratchReg1, liveMap))
                 irs.append(LDR(scratchReg1, scratchReg1, -WORDSIZE, "Default"))
                 irs.append(PUSH(scratchReg1))
               }
               case ar @ ArrayElem(ident, exprs) => {
//                 irs.append(MOV(R12, R3, "Default"))
                  irs.append(PUSH(R3))
                 // place array on stack for first index
                 irs.appendAll(getIntoTarget(ident, R3, liveMap))

                 val isChar = getSizeFromArrElem(ar) == 1

                 for (expr <- exprs) {
                   irs.appendAll(generateExprIR(expr, liveMap))
                   irs.append(POP(R10))
                   if (isChar) {
                     irs.append(BRANCH("_arrLoadB", "L"))
                   } else {
                     irs.append(BRANCH("_arrLoad", "L"))
                   }
                 }

                 // at this point R3 hold ths target array
                 irs.append(LDR(scratchReg1, R3, -WORDSIZE, "Default"))
                 irs.append(POP(R3))
                 irs.append(PUSH(scratchReg1))

                 if (isChar) {
                    widgets("arrLoadB") = collection.mutable.Set.empty
                 } else {
                   widgets("arrLoad") = collection.mutable.Set.empty
                 }
                 widgets("boundsCheck") = collection.mutable.Set.empty
               }
             }
            irs.toList
          }
        }
      }

      case expr: BinopExpr => {
        expr match {
          case AddExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), ADDREG(scratchReg1, scratchReg2, scratchReg1), PUSH(scratchReg1))  // TODO : check for overflow
          case SubExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), SUBREG(scratchReg1, scratchReg1, scratchReg2), PUSH(scratchReg1))  // TODO : check for overflow
          case MulExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), MUL(scratchReg1, scratchReg2), PUSH(scratchReg1)) // TODO : check for overflow
          case DivExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), DIV(scratchReg1, scratchReg2, locals)) // TODO : check for division by zero
          case ModExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), MOD(scratchReg1, scratchReg2, locals))
          case GTExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "GT"), MOVImm(scratchReg1,0, "LE"), PUSH(scratchReg1))
          case LTExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "LT"), MOVImm(scratchReg1,0, "GE"), PUSH(scratchReg1))
          case GTEQExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "GE"), MOVImm(scratchReg1,0, "LT"), PUSH(scratchReg1))
          case LTEQExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "LE"), MOVImm(scratchReg1,0, "GT"), PUSH(scratchReg1))
          case AndExpr(e1, e2) =>
            val L1 : List[IR] = generateExprIR(e1, liveMap)
            val L2 : List[IR] = generateExprIR(e2, liveMap)
            val label : String = getNewLabel()
            val L3 : List[IR] = List(POP(scratchReg2), POP(scratchReg1), AND(scratchReg1, scratchReg2, label), PUSH(scratchReg1))
            L1 ++ L2 ++ L3
          case OrExpr(e1, e2) =>
            val L1 : List[IR] = generateExprIR(e1, liveMap)
            val L2 : List[IR] = generateExprIR(e2, liveMap)
            val label : String = getNewLabel()
            val L3 : List[IR] = List(POP(scratchReg2), POP(scratchReg1), OR(scratchReg1, scratchReg2, label), PUSH(scratchReg1))
            L1 ++ L2 ++ L3

          case EQExpr(e1, e2) =>  generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "EQ"), MOVImm(scratchReg1,0, "NE"), PUSH(scratchReg1))
          case NEQExpr(e1, e2) => generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "NE"), MOVImm(scratchReg1,0, "EQ"), PUSH(scratchReg1))
        }
      }
      case arrElem @ ArrayElem(ident, exprs) => {
        val irs = ListBuffer.empty[IR]

//        irs.append(MOV(R12, R3, "Default"))
        irs.append(PUSH(R3)) // save R3

        // place array on stack for first index
        irs.appendAll(getIntoTarget(ident, R3, liveMap))

        val isChar = getSizeFromArrElem(arrElem) == 1

        for (expr <- exprs) {
          irs.appendAll(generateExprIR(expr, liveMap))
          irs.append(POP(R10))
          if (isChar) {
            irs.append(BRANCH("_arrLoadB", "L"))
          } else {
            irs.append(BRANCH("_arrLoad", "L"))
          }
        }

        irs.append(MOV(scratchReg1, R3, "Default"))
        irs.append(POP(R3)) // restore r3
        irs.append(PUSH(scratchReg1))
//        irs.append(MOV(R3, R12, "Default"))
        if (isChar) {
          widgets("arrLoadB") = collection.mutable.Set.empty
        } else {
          widgets("arrLoad") = collection.mutable.Set.empty
        }
        widgets("boundsCheck") = collection.mutable.Set.empty

        irs.toList
      }
//      case PairExpr() => TODO

    }
  }

  // dfs to find the number of variable assignment in statement body
  def findNumLocals(stat : Statement) : Int = {
    stat match {
      case _: VarDec => 1
      case ScopeStat(stat) => findNumLocals(stat)
      case While(_, doStat) => findNumLocals(doStat)
      case ConsecStat(first, next) => findNumLocals(first) + findNumLocals(next)
      case If(_, thenStat, elseStat) => findNumLocals(thenStat) + findNumLocals(elseStat)
      case _ => 0
    }
  }

  // writes instructions that calculate the value and place it on top of the stack
  def generateRvalue(rvalue: RValue, liveMap: SymbolTable[Location], localRegs : List[Reg], numParams : Int, lval : LValue) : List[IR] = {
    rvalue match {
      case expr: Expr => generateExprIR(expr, liveMap)
      //      case NewPair(expr1, expr2) => [] TODO
      case ArrayLiter(exprs) => {
        val irs = ListBuffer.empty[IR]
        val exprLen = exprs.length
        val saveParamRegs = willClobber(localRegs, liveMap)

        val size = lval match {
          case id@IdentValue(s) => {
            assert(id.st.isDefined, "id does not have symbol table")
            id.st.get.lookupAll(s).get match {
              case ArraySemType(CharSemType) => 1
              case _ => WORDSIZE
            }
          }
          case ar : ArrayElem => getSizeFromArrElem(ar)
//          case elem: PairElem => TODO
        }

        if (saveParamRegs) {
          irs.append(PUSHMul(paramRegs))
        }
        irs.append(MOVImm(R0, (exprLen * size) + WORDSIZE, "Default"))
        irs.append(BRANCH("malloc", "L"))
        irs.append(MOV(R12, R0, "Default"))

        if (saveParamRegs) {
          irs.append(POPMul(paramRegs))
        }

        irs.append(ADD(R12, R12, WORDSIZE, "Default"))

        // store size of array
        irs.append(MOVImm(scratchReg1, exprLen, "Default"))
        irs.append(STR(scratchReg1, R12, -WORDSIZE, "Default"))

        // set all the expr in mem
        for (i <- exprs.indices) {
          irs.appendAll(generateExprIR(exprs(i), liveMap))
          irs.append(POP(scratchReg1))
          irs.append(STR(scratchReg1, R12, i * size, "Default"))
        }
        irs.append(PUSH(R12))
        irs.toList
      }

      case Call(ident, lArgs) => {
        val localCount = liveMap.getNestedEntries()
        val irs = ListBuffer.empty[IR]


        // caller saved
        if (numParams > 0) {
          irs.append(PUSHMul(paramRegs.slice(0, numParams)))
        }
        val realCount = localCount - numParams
        if (realCount > 0) {
          irs.append(PUSHMul(localRegs.slice(0, realCount)))
        }

        for (i <- lArgs.length - 1 to 0 by -1) {
          irs.appendAll(generateExprIR(lArgs(i), liveMap)) // this leaves the value on top of stack for function call
        }

        for (i <- 0 until math.min(WORDSIZE, lArgs.length)) {
          irs.append(POP(paramRegs(i)))
        }

        irs.append(BRANCH(FUNCTION_PREFIX + ident, "L"))

        if (lArgs.length > WORDSIZE) {
          irs.append(ADD(SP, SP, (lArgs.length - WORDSIZE) * WORDSIZE, "Default"))
        }

        irs.append(MOV(scratchReg1, R0, "Default"))

        // caller saved
        if (realCount > 0) {
          irs.append(POPMul(localRegs.slice(0, realCount)))
        }
        if (numParams > 0) {
          irs.append(POPMul(paramRegs.slice(0, numParams)))
        }

        irs.append(PUSH(scratchReg1))
        irs.toList
      }
    }
  }

  def getSizeFromArrElem (arrElem : ArrayElem) : Int = {
    var arrType = arrElem.st.get.lookupAll(arrElem.ident).get
    var depth = arrElem.exprs.length

    while (depth > 0) {
      arrType match {
        case ArraySemType(t) => {
          arrType = t
        }
        case _ => throw new RuntimeException("should not reach here")
      }
      depth -= 1
    }

    arrType match {
      case SemTypes.CharSemType => 1
      case _ => 4
    }
  }

  // we are working with caller saved strategy
  // Parameters stored in r0-r3 and then on stack
  def generateFuncIR(func : Func, totalLocalRegs : List[Reg]): List[IR] = {
    val irs = ListBuffer.empty[IR]
    irs += Label(FUNCTION_PREFIX + func.ident)
    irs += PUSHMul(List(FP, LR))
    irs.append(MOV(FP, SP, "Default"))

    // set up function parameters
    val liveMap = new SymbolTable[Location](None)

    val numParams = func.params.length
    for (i <- 0 until numParams) {
      if (i < WORDSIZE) {
        liveMap.add(func.params(i).ident, paramRegs(i))
      } else {
        liveMap.add(func.params(i).ident, Stack((i - 2) * WORDSIZE))
      }
    }

    // setting up function local registers
    val localRegsBuilder = ListBuffer.empty[Reg]
    val totalNum = totalLocalRegs.length
    localRegsBuilder.appendAll(totalLocalRegs.slice(0,WORDSIZE))
    if (numParams <= WORDSIZE) {
      for (i <- (totalNum - (WORDSIZE - numParams)) until totalNum) {
        localRegsBuilder.append(totalLocalRegs(i))
      }
    }

    // this code should consume the statement body of the function (Caller saved convention)
    val numLocalsInBody = findNumLocals(func.stat)
    val localRegs = localRegsBuilder.toList
    val numLocalRegs = localRegs.length
    if (numLocalsInBody > numLocalRegs) {
      irs.append(SUB(SP, SP, (numLocalsInBody - numLocalRegs) * WORDSIZE))
    }
    val childLiveMap = new SymbolTable[Location](Some(liveMap))
    irs.appendAll(generateStatIR(func.stat, childLiveMap, localRegs, numParams))
    irs += POPMul(List(FP, PC))

    if (numLocalsInBody > numLocalRegs) {
      irs.append(ADD(SP, SP, (numLocalsInBody - numLocalRegs) * WORDSIZE, "Default"))
    }
    irs.append(LTORG)
    irs.toList
  }

  def generateStatIR(stat: Statement, liveMap : SymbolTable[Location], localRegs : List[Reg], numParams : Int): List[IR] = {
    stat match {

      case Exit(e) => generateExprIR(e, liveMap) ++ List(POP(R0), BRANCH("exit", "L"))

      case Skip => List.empty[IR]

      case varDec@VarDec(_, ident, rvalue) =>
        generateRvalue(rvalue, liveMap, localRegs, numParams, IdentValue(ident)(varDec.symbolTable, (0,0))) ++ assignLocal(ident, liveMap, localRegs, numParams)

      case Assign(lvalue, rvalue) => {
        lvalue match {
//          case elem: PairElem => // TODO
          case arrElem @ ArrayElem(ident, exprs) => {
            val irs = ListBuffer.empty[IR]
            irs.append(PUSH(R3))
//            irs.append(MOV(FP, SP, "Default"))

            irs.appendAll(getIntoTarget(ident, R3, liveMap))

            val isChar = getSizeFromArrElem(arrElem) == 1

            for (expr <- exprs.slice(0, exprs.length - 1)) {
              irs.appendAll(generateExprIR(expr, liveMap))
              irs.append(POP(R10))
              if (isChar) {
                irs.append(BRANCH("_arrLoadB", "L"))
              }
              else {
                irs.append(BRANCH("_arrLoad", "L"))
              }
            }

            // Now R3 contains the required array and last expr is what we want
            irs.appendAll(generateExprIR(exprs.last, liveMap))
            irs.append(POP(R10))
            irs.appendAll(generateRvalue(rvalue, liveMap, localRegs, numParams, lvalue))
            irs.append(POP(scratchReg1))

            if (isChar) {
              irs.append(BRANCH("_arrStoreB", "L"))
            }
            else {
              irs.append(BRANCH("_arrStore", "L"))
            }

//            irs.append(MOV(SP, FP, "Default"))
            irs.append(POP(R3))

            if (isChar) {
              widgets("arrStoreB") = collection.mutable.Set.empty
            } else {
              widgets("arrStore") = collection.mutable.Set.empty
            }
            widgets("boundsCheck") = collection.mutable.Set.empty


            irs.toList
          }
          case IdentValue(s) => {
            val irs = ListBuffer.empty[IR]
            irs.appendAll(generateRvalue(rvalue, liveMap, localRegs, numParams, lvalue))

            assert(liveMap.lookupAll(s).isDefined)
            liveMap.lookupAll(s).get match {
              case reg: Reg => irs.append(POP(reg))
              case Stack(offset) => irs.appendAll(List(POP(scratchReg1), STR(scratchReg1, SP, offset, "Default")))
            }
            irs.toList
          }
        }
      }


      case ConsecStat(first, next) => generateStatIR(first, liveMap, localRegs, numParams) ++ generateStatIR(next, liveMap, localRegs, numParams)

      case ScopeStat(stat) => generateStatIR(stat, new SymbolTable[Location](Some(liveMap)), localRegs, numParams)

      case While(cond, doStat) => {
      val label1 : String = getNewLabel()
      val label2 : String = getNewLabel()

      val whileIr = ListBuffer.empty[IR]
      whileIr.append(BRANCH(label2 , "Default"))
      whileIr.append(Label(label1))
      val doLiveMap = new SymbolTable[Location](Some(liveMap))
      whileIr.appendAll(generateStatIR(doStat, doLiveMap, localRegs, numParams))
      whileIr.append(Label(label2))
      whileIr.appendAll(generateExprIR(cond, liveMap))
      whileIr.append(POP(scratchReg1))
      whileIr.append(CMPImm(scratchReg1, 0))
      whileIr.append(BRANCH(label1, "NE"))

      whileIr.toList
    }

      case If(cond, thenStat, elseStat) => {
      val label0 : String = getNewLabel()
      val label1 : String = getNewLabel()

      val ifIr = ListBuffer.empty[IR]
      ifIr.appendAll(generateExprIR(cond, liveMap))
      ifIr.append(POP(scratchReg1))
      ifIr.append(CMPImm(scratchReg1, 0))
      ifIr.append(BRANCH(label1, "EQ"))
      val thenLiveMap = new SymbolTable[Location](Some(liveMap))
      ifIr.appendAll(generateStatIR(thenStat, thenLiveMap, localRegs, numParams))
      ifIr.append(BRANCH(label0,"Default"))
      ifIr.append(Label(label1))
      val elseLiveMap = new SymbolTable[Location](Some(liveMap))
      ifIr.appendAll(generateStatIR(elseStat, elseLiveMap, localRegs, numParams))
      ifIr.append(Label(label0))

      ifIr.toList
    }

      case Print(e, opType) => {
        assert(opType.isDefined, "Expr must have a type")
        getPrintIR(e, opType.get, liveMap, localRegs,ln = false)
      }

      case Println(e, opType) => {
        assert(opType.isDefined, "Expr must have a type")
        getPrintIR(e, opType.get, liveMap, localRegs,ln = true)
      }


      case Read(lvalue) => {
        lvalue match {
          case id@IdentValue(s) => {
            assert(id.st.isDefined, "Symbol table not associated with ident value")
            val idType = id.st.get.lookupAll(s)
            assert(idType.isDefined, "Ident"+ s + " not in symbol table ")
            idType.get match {
              case SemTypes.IntSemType => getReadIr(s,"i", liveMap, localRegs)
              case SemTypes.CharSemType => getReadIr(s,"c", liveMap, localRegs)
              case t => {
                println("Wrong type ", t)
                throw new RuntimeException("Cannot read into non int/char - should not get here")
              }
            }

          }
//          case elem: PairElem => TODO
          case ar @ ArrayElem(ident, exprs) => {
            val irs = ListBuffer.empty[IR]

            val isChar = getSizeFromArrElem(ar) == 1
//            irs.append(MOV(FP, SP, "Default"))
            irs.append(PUSH(R3))


            // place array on stack for first index
            irs.appendAll(getIntoTarget(ident, R3, liveMap))

            for (expr <- exprs) {
              irs.appendAll(generateExprIR(expr, liveMap))
              irs.append(POP(R10))
              if (isChar) {
                irs.append(BRANCH("_arrLoadB", "L"))
              } else {
                irs.append(BRANCH("_arrLoad", "L"))
              }
            }

            if (isChar) {
              widgets("arrLoadB") = collection.mutable.Set.empty
            } else {
              widgets("arrLoad") = collection.mutable.Set.empty
            }


            // at this point R3 - contains the accessed value arr[x][y]
            val shouldSave = willClobber(localRegs, liveMap)
            if (shouldSave) {
              irs.append(PUSHMul(paramRegs.slice(0,3)))
            }

            irs.append(MOV(R0, R3, "Default"))

            // find out what kind of read it is
            assert(ar.st.isDefined, "Array Elem does not have st attached to it")
            assert(ar.st.get.lookupAll(ident).isDefined, "array elem not in st")
            val flag = getArrType(ar.st.get.lookupAll(ident).get) match {
              case SemTypes.CharSemType => "c"
              case SemTypes.IntSemType => "i"
              case _ => throw new RuntimeException("Should not be possible to read into non int/char arr")
            }

            irs.append(BRANCH("_read" + flag, "L"))
            if (widgets.contains("read")) {
              widgets("read").add(flag)
            } else {
              widgets("read") = collection.mutable.Set(flag)
            }

            irs.append(MOV(R12, R0, "Default")) // R12 holds the value of the read

            irs.appendAll(getIntoTarget(ident, R3, liveMap))
            for (expr <- exprs.slice(0, exprs.length - 1)) {
              irs.appendAll(generateExprIR(expr, liveMap))
              irs.append(POP(R10))
              if (isChar) {
                irs.append(BRANCH("_arrLoadB", "L"))
              } else {
                irs.append(BRANCH("_arrLoad", "L"))
              }
            }
            // Now R3 contains the required array and last expr is what we want
            irs.appendAll(generateExprIR(exprs.last, liveMap))
            irs.append(POP(R10))
            irs.append(MOV(scratchReg1, R12, "Default"))

            if (isChar) {
              irs.append(BRANCH("_arrStoreB", "L"))
              widgets("arrStoreB") = collection.mutable.Set.empty
            } else {
              irs.append(BRANCH("_arrStore", "L"))
              widgets("arrStore") = collection.mutable.Set.empty

            }

            if (shouldSave) {
              irs.append(POPMul(paramRegs.slice(0,3)))
            }

            irs.append(POP(R3))
//            irs.append(MOV(SP, FP, "Default"))

            irs.toList
          }
        }
      }


      case Return(e) => generateExprIR(e, liveMap).appendedAll(List(POP(R0), POPMul(List(FP, PC))))

      case stat => {
        println(stat)
        null
      } // TODO
    }
  }

  def getArrType(arrType : SemType): SemType = {
    arrType match {
      case ArraySemType(t) => getArrType(t)
      case x => x
    }
  }

  def willClobber(localReg : List[Reg], liveMap : SymbolTable[Location]) : Boolean = {
    var i = 0
    while (i < localReg.length && i < liveMap.getNestedEntries()) {
      if (localReg(i) == R0) {
        return true
      }
      i += 1
    }
    !localReg.contains(R0)
  }

  def getPrintIR(e : Expr, expType : SemType, liveMap : SymbolTable[Location], localReg : List[Reg], ln : Boolean): List[IR] = {
    val irs = ListBuffer.empty[IR]

    val clobber = willClobber(localReg, liveMap)

    if (clobber) {
      irs.append(PUSHMul(paramRegs))
    }

    irs.appendAll(generateExprIR(e, liveMap))
    irs.append(POP(R0))

    val flag = expType match {
      case CharSemType => "c"
      case IntSemType => "i"
      case StringSemType => "s"
      case PairSemType(_, _) => "p"
      case BoolSemType => "b"
      case ArraySemType(t) =>{
        if (t == CharSemType) "s"
        else "p"
      }
      case _ => { // TODO ask possible
        assert(assertion = false, "not possible to print internal pair type")
        "not possible"
      }

    }

    irs.append(BRANCH("_print" + flag, "L"))
    if (widgets.contains("print")) {
      widgets("print").add(flag)
    } else {
      widgets("print") = collection.mutable.Set(flag)
    }

    if (ln) {
      irs.append(BRANCH("_println", "L"))
      widgets("printNewLine") = collection.mutable.Set.empty
    }

    if (clobber) {
      irs.append(POPMul(paramRegs))
    }
    irs.toList
  }

  def getReadIr(ident : String, flag : String, liveMap : SymbolTable[Location], localReg : List[Reg]) : List[IR] = {
    val irs = ListBuffer.empty[IR]

    val didClobber = willClobber(localReg, liveMap)

    if (didClobber) {
      irs.append(PUSHMul(paramRegs))
    }

    // R0 should hold the value of ident
    assert(liveMap.lookupAll(ident).isDefined, "Ident " + ident + "has not been defined")
    liveMap.lookupAll(ident).get match {
      case Stack(offset) => irs.append(LDR(R0, FP, offset, "Default"))
      case reg: Reg => irs.append(MOV(R0, reg, "Default"))
    }

    irs.append(BRANCH("_read" + flag, "L"))
    if (widgets.contains("read")) {
      widgets("read").add(flag)
    } else {
      widgets("read") = collection.mutable.Set(flag)
    }

    irs.append(MOV(scratchReg1, R0, "Default"))

    if (didClobber) {
      irs.append(POPMul(paramRegs))
    }

    liveMap.lookupAll(ident).get match {
      case Stack(offset) => irs.append(STR(scratchReg1, FP, offset, "Default"))
      case reg: Reg => irs.append(MOV(reg, scratchReg1, "Default"))
    }

    irs.toList
  }

  private def readChar(): List[IR] = {
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List(" %c"), stringNum))
    ir.append(Label("_readc"))
    ir.append(PUSH(LR))
    ir.append(STR(R0, SP, -1,"b"))
    ir.append(SUB(SP, SP, 1))
    ir.append(MOV(R1, SP, "Default"))
    ir.append(StringInit(R0,stringNum))
    stringNum += 1
    ir.append(BRANCH("scanf", "L"))
    ir.append(LDR(R0, SP, 0, "sb"))
    ir.append(ADD(SP, SP, 1, "Default"))
    ir.append(POP(PC))
    ir.toList
  }

  def readInt(): List[IR] = {
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List("%d"), stringNum))
    ir.append(Label("_readi"))
    ir.append(PUSH(LR))
    ir.append(PUSH(R0))
    ir.append(MOV(R1, SP, "Default"))
    ir.append(StringInit(R0,stringNum))
    stringNum += 1
    ir.append(BRANCH("scanf", "L"))
    ir.append(LDR(R0, SP, 0, "Default"))
    ir.append(ADD(SP, SP, WORDSIZE, "Default"))
    ir.append(POP(PC))
    ir.toList
  }

  def readAll(flag : String) : List[IR] = {
    flag match {
      case "c" => readChar()
      case "i" => readInt()
    }
  }

  def printAll(flag : String) : List[IR] = {
    flag match {
      case "c" => printBasic("c")
      case "i" => printBasic("i")
      case "s" => printString()
      case "b" => printBool()
      case "p" => printPointer()
   }
  }


  private def printPointer() : List[IR] = {
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List("%p"), stringNum))
    ir.append(Label("_printp"))
    ir.append(PUSH(LR))
    ir.append(MOV(R1, R0, "Default"))
    ir.append(StringInit(R0, stringNum))
    stringNum += 1
    ir.append(BRANCH("printf", "L"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BRANCH("fflush", "L"))
    ir.append(POP(PC))
    ir.toList
  }

  private def printBool() : List[IR] = {
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List("false", "true", "%.*s"), stringNum))
    ir.append(Label("_printb"))
    ir.append(PUSH(LR))
    ir.append(CMPImm(R0, 0))
    val label0 = getNewLabel()
    val label1 = getNewLabel()
    ir.append(BRANCH(label0, "NE"))
    ir.append(StringInit(R2, stringNum))
    ir.append(BRANCH(label1, "Default"))
    ir.append(Label(label0))
    ir.append(StringInit(R2, stringNum + 1))
    ir.append(Label(label1))
    ir.append(LDR(R1, R2, -WORDSIZE, "Default"))
    ir.append(StringInit(R0, stringNum + 2))
    ir.append(BRANCH("printf", "L"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BRANCH("fflush", "L"))
    ir.append(POP(PC))
    stringNum += 3
    ir.toList
  }

  private def printString() : List[IR] = {
    val format = "%.*s"
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List(format), stringNum))
    ir.append(Label("_prints"))
    ir.append(PUSH(LR))
    ir.append(MOV(R2, R0, "Default"))
    ir.append(LDR(R1, R0, -WORDSIZE, "Default"))
    ir.append(StringInit(R0, stringNum))
    stringNum += 1
    ir.append(BRANCH("printf", "L"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BRANCH("fflush", "L"))
    ir.append(POP(PC))
    ir.toList
  }

  private def printBasic(flag : String): List[IR] ={
    val format : String = flag match {
      case "c" => "%c"
      case "i" => "%d"
      case  _  => "Error: Invalid!!"
    }
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List(format), stringNum))
    ir.append(Label("_print" + flag))
    ir.append(PUSH(LR))
    ir.append(MOV(R1, R0, "Default"))
    ir.append(StringInit(R0, stringNum))
    stringNum += 1
    ir.append(BRANCH("printf", "L"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BRANCH("fflush", "L"))
    ir.append(POP(PC))
    ir.toList
  }

  def printNewLine() : List[IR] = {
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List(""), stringNum))
    ir.append(Label("_println"))
    ir.append(PUSH(LR))
    ir.append(StringInit(R0, stringNum))
    ir.append(BRANCH("puts", "L"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BRANCH("fflush", "L"))
    ir.append(POP(PC))
    stringNum += 1
    ir.toList
  }

  def getIntoTarget(name: String, target : Reg, liveMap : SymbolTable[Location]): List[IR] = {
    val location = liveMap.lookupAll(name)
    assert(location.isDefined, name + " must be defined in live map for getting its val")
    location.get match {
      case src: Reg => List(MOV(target, src, "Default"))
      case Stack(offset) => List(LDR(scratchReg1, FP, offset, "Default"), MOV(target, scratchReg1, "Default"))
    }
  }

  def assignLocal(ident: String, liveMap: SymbolTable[Location], localRegs : List[Reg], numParams : Int): List[IR] = {
    val localCount = liveMap.getNestedEntries()
    assert(liveMap.lookup(ident).isEmpty, "First assignment of " + ident + " in child scope")
    val realLocal = localCount - numParams
    if (realLocal < localRegs.size) {
      liveMap.add(ident, localRegs(realLocal))
      List(POP(localRegs(realLocal)))
    } else {
      val offset = (realLocal - localRegs.size + 1)*(-WORDSIZE)
      liveMap.add(ident, Stack(offset))
      List(POP(scratchReg1), STR(scratchReg1, FP, offset, "Default"))
    }
  }

  def getNewLabel(): String = {
    val label : String = ".L" + labelOrder.toString
    labelOrder += 1
    label
  }

  def optimisePushPop(irs : List[IR]) : List[IR] = {
    val newIRs = ListBuffer.empty[IR]
    var i = 0
    while (i < irs.length) {
      val ir = irs(i)
      if (i < (irs.length - 1)) {
        ir match {
          case PUSH(reg1) =>
            val irNext = irs(i + 1)
            irNext match {
              case POP(reg2) =>
                if (reg1 == reg2) {
                  i += 2
                }
                else {
                  newIRs.append(MOV(reg2, reg1, "Default"))
                  i += 2
                }
              case _ =>
                newIRs.append(ir)
                i += 1
            }
          case _ =>
            newIRs.append(ir)
            i += 1
        }
      } else {
        newIRs.append(ir)
        i += 1
      }
    }
    newIRs.toList
  }

  def clearLiveMap(liveMap: SymbolTable[Location]) : Unit = liveMap.map.clear()

  def boundsCheck() : List[IR] = {
    val ir = new ListBuffer[IR]
    ir.append(Data(List("fatal error: array index %d out of bounds\n"), stringNum))
    ir.append(Label("_boundsCheck"))
    ir.append(StringInit(R0, stringNum))
    stringNum += 1
    ir.append(BRANCH("printf", "L"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BRANCH("fflush", "L"))
    ir.append(MOVImm(R0, 255, "Default"))
    ir.append(BRANCH("exit", "L"))
    ir.toList
  }

  def arrLoad(): List[IR] = {
    val ir = new ListBuffer[IR]
    ir.append(Label("_arrLoad"))
    ir.append(PUSH(LR))
    ir.append(CMPImm(R10, 0))
    ir.append(MOV(R1, R10, "LT"))
    ir.append(BRANCH("_boundsCheck", "LLT"))
    ir.append(LDR(LR, R3, -WORDSIZE, "Default"))
    ir.append(CMP(R10, LR))
    ir.append(MOV(R1, R10, "GE"))
    ir.append(BRANCH("_boundsCheck", "LGE"))
    ir.append(LDR(R3, R10, 2, "index"))
    ir.append(POP(PC))
    ir.toList
  }


  def arrLoadB() : List[IR] = {
    val ir = new ListBuffer[IR]
    ir.append(Label("_arrLoadB"))
    ir.append(PUSH(LR))
    ir.append(CMPImm(R10, 0))
    ir.append(MOV(R1, R10, "LT"))
    ir.append(BRANCH("_boundsCheck", "LLT"))
    ir.append(LDR(LR, R3, -WORDSIZE, "Default"))
    ir.append(CMP(R10, LR))
    ir.append(MOV(R1, R10, "GE"))
    ir.append(BRANCH("_boundsCheck", "LGE"))
    ir.append(LDR(R3, R10, 0, "sbReg"))
    ir.append(POP(PC))
    ir.toList
  }

  def arrStore(): List[IR] = {
    val ir = new ListBuffer[IR]
    ir.append(Label("_arrStore"))
    ir.append(PUSH(LR))
    ir.append(CMPImm(R10, 0))
    ir.append(MOV(R1, R10, "LT"))
    ir.append(BRANCH("_boundsCheck", "LLT"))
    ir.append(LDR(LR, R3, -WORDSIZE, "Default"))
    ir.append(CMP(R10, LR))
    ir.append(MOV(R1, R10, "GE"))
    ir.append(BRANCH("_boundsCheck", "LGE"))
    ir.append(STOREINDEX(scratchReg1, R3, R10, 2))
    ir.append(POP(PC))
    ir.toList
  }

  def arrStoreB(): List[IR] = {
    val ir = new ListBuffer[IR]
    ir.append(Label("_arrStoreB"))
    ir.append(PUSH(LR))
    ir.append(CMPImm(R10, 0))
    ir.append(MOV(R1, R10, "LT"))
    ir.append(BRANCH("_boundsCheck", "LLT"))
    ir.append(LDR(LR, R3, -WORDSIZE, "Default"))
    ir.append(CMP(R10, LR))
    ir.append(MOV(R1, R10, "GE"))
    ir.append(BRANCH("_boundsCheck", "LGE"))
    ir.append(STOREINDEXB(scratchReg1, R3, R10))
    ir.append(POP(PC))
    ir.toList
  }
//  def getStringsData(): List[String] = strings.toList

}