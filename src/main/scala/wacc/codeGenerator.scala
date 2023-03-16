package wacc

import wacc.AST._
import wacc.ClassTable.{ClassDefinition, ClassTable}
import wacc.IR._
import wacc.Registers._
import wacc.SemTypes._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class codeGenerator(program: Program, peephole: Boolean, inlineable: Boolean) {

  private val paramRegs = List(R0, R1, R2, R3)
  private var labelOrder = 0
  private val FUNCTION_PREFIX = "wacc_"
  private val WORD_SIZE = 4
  private val MAX_INLINE_LENGTH = 15

  private val strings: ListBuffer[String] = ListBuffer.empty[String]
  private var stringNum = 0

  private val widgets = collection.mutable.Set.empty[Widget]

  // defining structure of classes and structs
  private val structTable: StructTable.StructTable = program.structTable.get
  private val classTable: ClassTable = program.classTable.get
  private var curClassName: Option[String] = None

  // mangling class parameters, fields and methods
  private final val CLASS_METHOD_PREFIX = "wacc_class_"
  private final val CLASS_FIELD_PREFIX = "this."

  // generate assembly for entire program
  def generateProgIR(): List[IR] = {
    var irs = ListBuffer.empty[IR]
    // assembly hygiene
    irs += Global(List("main"))

    // this is consuming the *Main* body of the program --> R7 is a class pointer
    val localRegs: List[Reg] = List(R4, R5, R6, R0, R1, R2, R3)
    val numLocalRegs = localRegs.length

    irs += Label("main")
    irs += PUSHMul(List(FP, LR))
    irs.append(MOV(FP, SP, DEFAULT))
    val numLocalsInMain = findNumLocals(program.stat)
    if (numLocalsInMain > numLocalRegs) {
      irs.append(SUB(SP, SP, (numLocalsInMain - numLocalRegs) * WORD_SIZE))
    }
    val liveMap = new SymbolTable[Location](None)
    irs.appendAll(generateStatIR(program.stat, liveMap, localRegs, 0))

    if (numLocalsInMain > numLocalRegs) {
      irs.append(ADD(SP, SP, (numLocalsInMain - numLocalRegs) * WORD_SIZE, DEFAULT))
    }
    irs += MOVImm(R0, 0, DEFAULT)
    irs += POPMul(List(FP, PC))

    // generate assembly and optimise program for program functions
    if (inlineable) {
      irs = inlineAllFunctions(irs, program.funcs, localRegs)
    } else {
      program.funcs.map(func => irs.appendAll(generateFuncIR(func, localRegs)))
    }

    // generate assembly for class Methods
    for (waccClass <- program.classes) {
      curClassName = Some(waccClass.name) // so that method knows which class it is in
      for (method <- waccClass.methods) {
        irs.appendAll(generateFuncIR(method.func, localRegs))
      }
      curClassName = None
    }

    irs.prepend(Data(strings.toList, 0))

    if (peephole) {
      irs = optimisePeepHole(irs)
    }

    // add all required widgets
    widgets.foreach(w => irs.appendAll(w.getIR()))

    irs.toList
  }

  private def getMemberIntoTarget(ident: String, dst: Reg): List[IR] = {
    val irs = ListBuffer.empty[IR]
    assert(curClassName.isDefined, "This must being used in a class")

    val classDefn: ClassDefinition = classTable.lookup(curClassName.get).get
    val offset: Int = classDefn.getFieldOffset(ident).get
    val isChar: Boolean = classDefn.getFieldType(ident).get == CharSemType

    if (isChar) {
      irs.append(LDR(dst, CP, offset, BYTECONSTOFFSET))
    } else {
      irs.append(LDR(dst, CP, offset, DEFAULT))
    }
    irs.toList
  }

  // this function writes instructions that calculate the value of the expression
  // and leave them on top of the stack, only use R8 and R9 as temporaries
  private def generateExprIR(expr: Expr, liveMap: SymbolTable[Location], localRegs: List[Reg]): List[IR] = {
    expr match {
      case IntExpr(x) => List(MOVImm(scratchReg1, x, DEFAULT), PUSH(scratchReg1))
      case BoolExpr(b) => {
        var value = 0
        if (b) value = 1
        List(MOVImm(scratchReg1, value, DEFAULT), PUSH(scratchReg1))
      }
      case CharExpr(c) => {
        List(MOVImm(scratchReg1, c.toInt, DEFAULT), PUSH(scratchReg1))
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

      case IfExpr(cond, thenExpr, elseExpr) => {
        val label0: String = getNewLabel
        val label1: String = getNewLabel

        val ifIr = ListBuffer.empty[IR]
        ifIr.appendAll(generateExprIR(cond, liveMap, localRegs))
        ifIr.append(POP(scratchReg1))
        ifIr.append(CMPImm(scratchReg1, 0))
        ifIr.append(BRANCH(label1, EQ))
        val thenLiveMap = new SymbolTable[Location](Some(liveMap))
        ifIr.appendAll(generateExprIR(thenExpr, thenLiveMap, localRegs))
        ifIr.append(BRANCH(label0, DEFAULT))
        ifIr.append(Label(label1))
        val elseLiveMap = new SymbolTable[Location](Some(liveMap))
        ifIr.appendAll(generateExprIR(elseExpr, elseLiveMap, localRegs))
        ifIr.append(Label(label0))

        ifIr.toList
      }
      case ThisExpr(ident) => {
        getMemberIntoTarget(ident, scratchReg1).appended(PUSH(scratchReg1))
      }

      case expr: UnopExpr => {
        expr match {
          case ChrExpr(e) => {
            val irs = ListBuffer.empty[IR]
            irs.appendAll(generateExprIR(e, liveMap, localRegs))
            irs.append(POP(scratchReg1))
            irs.append(TRUNCATE(scratchReg1, scratchReg1, 127)) // and r8, r8, #127
            irs.append(PUSH(scratchReg1))
            irs.toList
          }
          case OrdExpr(e) => generateExprIR(e, liveMap, localRegs)
          case NegExpr(e) => {
            val ir = new ListBuffer[IR]
            ir.appendAll(generateExprIR(e, liveMap, localRegs))
            ir.appendAll(List(POP(scratchReg1), NEG(scratchReg1, scratchReg1), PUSH(scratchReg1)))
            widgets.add(errOverflow)
            ir.toList
          }
          case NotExpr(e) => generateExprIR(e, liveMap, localRegs) ++ List(POP(scratchReg1), NOT(scratchReg1, scratchReg1), PUSH(scratchReg1))
          case LenExpr(e) => {
            val irs = ListBuffer.empty[IR]
            e match {
              case IdentExpr(ident) => {
                irs.appendAll(getIntoTarget(ident, scratchReg1, liveMap))
                irs.append(LDR(scratchReg1, scratchReg1, -WORD_SIZE, DEFAULT))
                irs.append(PUSH(scratchReg1))
              }
              case ThisExpr(ident) => {
                irs.appendAll(getMemberIntoTarget(ident, scratchReg1))
                irs.append(LDR(scratchReg1, scratchReg1, -WORD_SIZE, DEFAULT))
                irs.append(PUSH(scratchReg1)) // scratchReg1 now holds ths class member
              }
              case ar@ArrayElem(ident, exprs) => {
                //                 irs.append(MOV(R12, R3, DEFAULT))
                irs.append(PUSH(R3))
                // place array on stack for first index
                irs.appendAll(getIntoTarget(ident, R3, liveMap))
                val isChar = arrayElemIsChar(ar)

                for (expr <- exprs) {
                  irs.appendAll(generateExprIR(expr, liveMap, localRegs))
                  irs.append(POP(R10))
                  if (isChar) {
                    irs.append(BRANCH("_arrLoadB", L))
                  } else {
                    irs.append(BRANCH("_arrLoad", L))
                  }
                }

                // at this point R3 hold ths target array
                irs.append(LDR(scratchReg1, R3, -WORD_SIZE, DEFAULT))
                irs.append(POP(R3))
                irs.append(PUSH(scratchReg1))


                if (isChar) {
                  widgets.add(arrLoadB)
                } else {
                  widgets.add(arrLoad)
                }
              }
              case _ => {}
            }
            irs.toList
          }
        }
      }

      case expr: BinopExpr => {
        expr match {
          case AddExpr(e1, e2) => {
            val ir = new ListBuffer[IR]
            ir.appendAll(generateExprIR(e1, liveMap, localRegs))
            ir.appendAll(generateExprIR(e2, liveMap, localRegs))
            ir.appendAll(List(POP(scratchReg2), POP(scratchReg1), ADDREG(scratchReg1, scratchReg2, scratchReg1), PUSH(scratchReg1)))
            widgets.add(errOverflow)
            ir.toList
          }
          case SubExpr(e1, e2) => {
            val ir = new ListBuffer[IR]
            ir.appendAll(generateExprIR(e1, liveMap, localRegs))
            ir.appendAll(generateExprIR(e2, liveMap, localRegs))
            ir.appendAll(List(POP(scratchReg2), POP(scratchReg1), SUBREG(scratchReg1, scratchReg1, scratchReg2), PUSH(scratchReg1)))
            widgets.add(errOverflow)
            ir.toList
          }
          case MulExpr(e1, e2) => {
            val ir = new ListBuffer[IR]
            ir.appendAll(generateExprIR(e1, liveMap, localRegs))
            ir.appendAll(generateExprIR(e2, liveMap, localRegs))
            ir.appendAll(List(POP(scratchReg2), POP(scratchReg1), MUL(scratchReg1, scratchReg2), PUSH(scratchReg1)))
            widgets.add(errOverflow)
            ir.toList
          }
          case DivExpr(e1, e2) => {
            val ir = new ListBuffer[IR]
            ir.appendAll(generateExprIR(e1, liveMap, localRegs))
            ir.appendAll(generateExprIR(e2, liveMap, localRegs))
            ir.appendAll(List(POP(scratchReg2), POP(scratchReg1), DIV(scratchReg1, scratchReg2, willClobber(localRegs, liveMap))))
            widgets.add(errDivZero)
            ir.toList
          }
          case ModExpr(e1, e2) => {
            val ir = new ListBuffer[IR]
            ir.appendAll(generateExprIR(e1, liveMap, localRegs))
            ir.appendAll(generateExprIR(e2, liveMap, localRegs))
            ir.appendAll(List(POP(scratchReg2), POP(scratchReg1), MOD(scratchReg1, scratchReg2, willClobber(localRegs, liveMap))))
            widgets.add(errDivZero)
            ir.toList
          }
          case GTExpr(e1, e2) =>
            generateExprIR(e1, liveMap, localRegs) ++ generateExprIR(e2, liveMap, localRegs) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, GT), MOVImm(scratchReg1, 0, LE), PUSH(scratchReg1))
          case LTExpr(e1, e2) =>
            generateExprIR(e1, liveMap, localRegs) ++ generateExprIR(e2, liveMap, localRegs) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, LT), MOVImm(scratchReg1, 0, GE), PUSH(scratchReg1))
          case GTEQExpr(e1, e2) =>
            generateExprIR(e1, liveMap, localRegs) ++ generateExprIR(e2, liveMap, localRegs) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, GE), MOVImm(scratchReg1, 0, LT), PUSH(scratchReg1))
          case LTEQExpr(e1, e2) =>
            generateExprIR(e1, liveMap, localRegs) ++ generateExprIR(e2, liveMap, localRegs) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, LE), MOVImm(scratchReg1, 0, GT), PUSH(scratchReg1))
          case AndExpr(e1, e2) =>
            val L1: List[IR] = generateExprIR(e1, liveMap, localRegs)
            val L2: List[IR] = generateExprIR(e2, liveMap, localRegs)
            val label: String = getNewLabel
            val L3: List[IR] = List(POP(scratchReg2), POP(scratchReg1), AND(scratchReg1, scratchReg2, label), PUSH(scratchReg1))
            L1 ++ L2 ++ L3
          case OrExpr(e1, e2) =>
            val L1: List[IR] = generateExprIR(e1, liveMap, localRegs)
            val L2: List[IR] = generateExprIR(e2, liveMap, localRegs)
            val label: String = getNewLabel
            val L3: List[IR] = List(POP(scratchReg2), POP(scratchReg1), OR(scratchReg1, scratchReg2, label), PUSH(scratchReg1))
            L1 ++ L2 ++ L3

          case EQExpr(e1, e2) => generateExprIR(e1, liveMap, localRegs) ++ generateExprIR(e2, liveMap, localRegs) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, EQ), MOVImm(scratchReg1, 0, NE), PUSH(scratchReg1))
          case NEQExpr(e1, e2) => generateExprIR(e1, liveMap, localRegs) ++ generateExprIR(e2, liveMap, localRegs) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, NE), MOVImm(scratchReg1, 0, EQ), PUSH(scratchReg1))
        }
      }

      case structElem@StructElem(ident, field) => {
        val irs = ListBuffer.empty[IR]

        val identType = structElem.st.get.lookupAll(ident).get
        identType match {
          case StructSemType(structName) => {
            val offset = structTable.lookup(structName).get.getOffset(field).get
            irs.appendAll(getIntoTarget(ident, scratchReg1, liveMap))
            if (structTable.lookup(structName).get.lookup(field).get == CharSemType) {
              irs.append(LDR(scratchReg2, scratchReg1, offset, BYTECONSTOFFSET))
            } else {
              irs.append(LDR(scratchReg2, scratchReg1, offset, DEFAULT))
            }
            irs.append(PUSH(scratchReg2))
          }
          case _ => throw new RuntimeException()
        }

        irs.toList
      }


      case classElem@ClassElem(ident, member) => {
        val irs = ListBuffer.empty[IR]

        val identType = classElem.st.get.lookupAll(ident).get
        identType match {
          case ClassSemType(className) => {
            irs.append(PUSH(CP))
            irs.appendAll(getIntoTarget(ident, CP, liveMap)) // set class pointer
            val tempName = curClassName
            curClassName = Some(className)
            irs.appendAll(getMemberIntoTarget(member, scratchReg1))
            curClassName = tempName
            irs.append(POP(CP))
            irs.append(PUSH(scratchReg1))
          }
          case _ => throw new RuntimeException()
        }
        irs.toList
      }

      case arrElem@ArrayElem(ident, exprs) => {
        val irs = ListBuffer.empty[IR]

        irs.append(PUSH(R3)) // save R3

        // place array on stack for first index
        irs.appendAll(getIntoTarget(ident, R3, liveMap))

        val isChar = arrayElemIsChar(arrElem)

        for (i <- 0 until exprs.length - 1) {
          irs.appendAll(generateExprIR(exprs(i), liveMap, localRegs))
          irs.append(POP(R10))
          irs.append(BRANCH("_arrLoad", L))
        }
        irs.appendAll(generateExprIR(exprs.last, liveMap, localRegs))
        irs.append(POP(R10))
        if (isChar) {
          irs.append(BRANCH("_arrLoadB", L))
          widgets.add(arrLoadB)
        } else {
          irs.append(BRANCH("_arrLoad", L))
        }

        irs.append(MOV(scratchReg1, R3, DEFAULT))
        irs.append(POP(R3)) // restore r3
        irs.append(PUSH(scratchReg1))
        widgets.add(arrLoad)

        irs.toList
      }

      case PairExpr() => List(MOVImm(scratchReg1, 0, DEFAULT), PUSH(scratchReg1))

    }
  }

  // dfs to find the number of variable assignment in statement body
  private def findNumLocals(stat: Statement): Int = {
    stat match {
      case _: VarDec => 1
      case ScopeStat(stat) => findNumLocals(stat)
      case While(_, doStat) => findNumLocals(doStat)
      case ConsecStat(first, next) => findNumLocals(first) + findNumLocals(next)
      case If(_, thenStat, elseStat) => findNumLocals(thenStat) + findNumLocals(elseStat)
      case _ => 0
    }
  }

  private def exprIsChar(expr: Expr): Boolean = {
    expr match {
      case CharExpr(_) => true
      case ChrExpr(_) => true
      case id@IdentExpr(ident) => {
        val ty: Option[SemType] = id.st.get.lookupAll(ident)
        ty.get match {
          case SemTypes.CharSemType => true
          case _ => false
        }
      }
      case structElem@StructElem(ident, field) => {
        val structName = structElem.st.get.lookupAll(ident).get match {
          case StructSemType(structName) => structName
          case _ => throw new RuntimeException("shoudl not be possible")
        }

        structTable.lookup(structName).get.lookup(field).get == CharSemType
      }

      case classElem@ClassElem(ident, member) => {
        val className = classElem.st.get.lookupAll(ident).get match {
          case ClassSemType(className) => className
          case _ => throw new RuntimeException("shoudl not be possible")
        }

        classTable.lookup(className).get.getFieldType(member).get == CharSemType
      }

      case arrElem: ArrayElem => arrayElemIsChar(arrElem)
      case th@ThisExpr(ident) => {
        val ty: Option[SemType] = th.st.get.lookupAll(CLASS_FIELD_PREFIX + ident)
        ty.get match {
          case SemTypes.CharSemType => true
          case _ => false
        }
      }
      case _ => false
    }
  }

  private def storeExprOnStackAndPushPointer(expr: Expr, localRegs: List[Reg], liveMap: SymbolTable[Location]): List[IR] = {
    val irs = ListBuffer.empty[IR]
    val saveParamRegs = willClobber(localRegs, liveMap)

    if (saveParamRegs) {
      irs.append(PUSHMul(paramRegs))
    }

    val isChar = exprIsChar(expr)
    val size = if (isChar) 1 else WORD_SIZE

    irs.append(MOVImm(R0, size, DEFAULT))
    irs.append(BRANCH("malloc", L))
    irs.append(MOV(R12, R0, DEFAULT))

    if (saveParamRegs) {
      irs.append(POPMul(paramRegs))
    }
    irs.appendAll(generateExprIR(expr, liveMap, localRegs))
    irs.append(POP(scratchReg1))
    if (isChar) {
      irs.append(STR(scratchReg1, R12, 0, BYTECONSTOFFSET))
    } else {
      irs.append(STR(scratchReg1, R12, 0, DEFAULT))
    }
    irs.append(PUSH(R12))
    irs.toList
  }

  def lvalStructName(lval: LValue): String = {
    lval match {
      case id@IdentValue(s) => {
        var ident = s
        if (curClassName.isDefined) {
          ident = CLASS_FIELD_PREFIX + s
        }
        id.st.get.lookupAll(ident).get match {
          case StructSemType(ident) => ident
          case _ => throw new RuntimeException("Should not be possible")
        }
      }
      case elem: PairElem => {
        elem match {
          case f: Fst => {
            f.ty match {
              case StructSemType(ident) => ident
              case _ => throw new RuntimeException("Should not be possible")
            }
          }
          case s: Snd => {
            s.ty match {
              case StructSemType(ident) => ident
              case _ => throw new RuntimeException("Should not be possible")
            }
          }
        }
      }
      case structElem@StructElem(ident, field) => {

        val structName = structElem.st.get.lookupAll(ident).get match {
          case StructSemType(structName) => structName
          case _ => throw new RuntimeException("should not be possible")
        }

        structTable.lookup(structName).get.lookup(field).get match {
          case StructSemType(ident) => ident
          case _ => throw new RuntimeException("Should not be possible")
        }
      }

      case classElem@ClassElem(ident, member) => {

        val className = classElem.st.get.lookupAll(ident).get match {
          case ClassSemType(className) => className
          case _ => throw new RuntimeException("should not be possible")
        }

        classTable.lookup(className).get.getFieldType(member).get match {
          case StructSemType(structName) => structName
          case _ => throw new RuntimeException("Should not be possible")
        }

      }

      case arrElem: ArrayElem => {
        getArrElemType(arrElem) match {
          case StructSemType(ident) => ident
          case _ => throw new RuntimeException("Should not be possible")
        }
      }
    }
  }

  private def lvalCharArraySize(lval: LValue): Int = {
    lval match {
      case id@IdentValue(s) => {
        id.st.get.lookupAll(s).get match {
          case ArraySemType(CharSemType) => 1
          case _ => WORD_SIZE
        }
      }
      case structElem@StructElem(ident, field) => {
        structElem.st.get.lookupAll(ident).get match {
          case StructSemType(structName) => {
            structTable.lookup(structName).get.lookup(field).get match {
              case ArraySemType(t) => if (t == CharSemType) 1 else 4
              case _ => 4
            }
          }
          case _ => 4
        }
      }

      case classElem@ClassElem(ident, member) => {
        classElem.st.get.lookupAll(ident).get match {
          case ClassSemType(className) => {
            classTable.lookup(className).get.getFieldType(member).get match {
              case ArraySemType(t) => if (t == CharSemType) 1 else 4
              case _ => 4
            }
          }
          case _ => 4
        }
      }

      case arrElem: ArrayElem => {
        getArrElemType(arrElem) match {
          case ArraySemType(t) => if (t == CharSemType) 1 else 4
          case _ => 4
        }
      }
      case elem: PairElem => {
        if (elem match {
          case f: Fst => f.ty == ArraySemType(CharSemType)
          case s: Snd => s.ty == ArraySemType(CharSemType)
        }) 1 else WORD_SIZE
      }
    }
  }


  // writes instructions that calculate the value and place it on top of the stack
  private def generateRvalue(rvalue: RValue, liveMap: SymbolTable[Location], localRegs: List[Reg], numParams: Int, lval: LValue): List[IR] = {
    rvalue match {
      case elem: PairElem => {
        val irs = ListBuffer.empty[IR]
        irs.appendAll(getIRForPairElem(elem, liveMap, localRegs))
        irs.append(POP(scratchReg2))
        val isChar: Boolean = lval match {
          case id@IdentValue(s) => {
            id.st.get.lookupAll(s).get match {
              case SemTypes.CharSemType => true
              case _ => false
            }
          }
          case elem: PairElem => {
            elem match {
              case fst: Fst => fst.ty == CharSemType
              case snd: Snd => snd.ty == CharSemType
            }
          }

          case structElem@StructElem(ident, field) =>
            structElem.st.get.lookupAll(ident).get match {
              case StructSemType(structName) => structTable.lookup(structName).get.lookup(field).get == CharSemType
              case _ => throw new RuntimeException("should not reach here")
            }

          case classElem@ClassElem(ident, member) => {

            val className = classElem.st.get.lookupAll(ident).get match {
              case ClassSemType(className) => className
              case _ => throw new RuntimeException("should not be possible")
            }

            classTable.lookup(className).get.getFieldType(member).get == CharSemType
          }

          case arrElem: ArrayElem => arrayElemIsChar(arrElem)
        }
        if (isChar) {
          irs.append(LDR(scratchReg1, scratchReg2, 0, BYTECONSTOFFSET))
        } else {
          irs.append(LDR(scratchReg1, scratchReg2, 0, DEFAULT))
        }
        irs.append(PUSH(scratchReg1))
        irs.toList
      }
      case expr: Expr => generateExprIR(expr, liveMap, localRegs)
      case NewPair(expr1, expr2) => {
        val irs = ListBuffer.empty[IR]
        irs.appendAll(storeExprOnStackAndPushPointer(expr1, localRegs, liveMap))
        irs.appendAll(storeExprOnStackAndPushPointer(expr2, localRegs, liveMap))

        val saveParamRegs = willClobber(localRegs, liveMap)

        if (saveParamRegs) {
          irs.append(PUSHMul(paramRegs))
        }

        irs.append(MOVImm(R0, 2 * WORD_SIZE, DEFAULT))
        irs.append(BRANCH("malloc", L))
        irs.append(MOV(R12, R0, DEFAULT))

        if (saveParamRegs) {
          irs.append(POPMul(paramRegs))
        }

        irs.append(POP(scratchReg1))
        irs.append(STR(scratchReg1, R12, 4, DEFAULT))
        irs.append(POP(scratchReg1))
        irs.append(STR(scratchReg1, R12, 0, DEFAULT))
        irs.append(PUSH(R12))
        irs.toList
      }

      case NewClass(className, exprs) => {
        val irs = ListBuffer.empty[IR]

        val classDefn: ClassDefinition = classTable.lookup(className).get
        val size = classDefn.getClassSize

        val saveParamRegs = willClobber(localRegs, liveMap)

        if (saveParamRegs) {
          irs.append(PUSHMul(paramRegs))
        }

        irs.append(MOVImm(R0, size, DEFAULT))
        irs.append(BRANCH("malloc", L))
        irs.append(MOV(R12, R0, DEFAULT))

        if (saveParamRegs) {
          irs.append(POPMul(paramRegs))
        }

        // now setting up class field members
        irs.append(PUSH(CP))
        irs.append(MOV(CP, R12, DEFAULT))

        // R12 is now the pointer to the class in memory
        // setting up class parameters
        val params = classDefn.getParams
        for (i <- exprs.indices) {
          irs.appendAll(generateExprIR(exprs(i), liveMap, localRegs))
          irs.append(POP(scratchReg1))

          val offset = classDefn.getFieldOffset(params(i).ident).get
          if (classDefn.getFieldType(params(i).ident).get == CharSemType) {
            irs.append(STR(scratchReg1, CP, offset, BYTECONSTOFFSET))
          } else {
            irs.append(STR(scratchReg1, CP, offset, DEFAULT))
          }
        }

        val tempName = curClassName
        curClassName = Some(className)
        for (varDec <- classDefn.getFields) {
          irs.appendAll(generateRvalue(varDec.rvalue, liveMap, localRegs, numParams,
            IdentValue(varDec.ident)(varDec.symbolTable, varDec.pos)))
          irs.append(POP(scratchReg1))

          val offset = classDefn.getFieldOffset(varDec.ident).get
          if (classDefn.getFieldType(varDec.ident).get == CharSemType) {
            irs.append(STR(scratchReg1, CP, offset, BYTECONSTOFFSET))
          } else {
            irs.append(STR(scratchReg1, CP, offset, DEFAULT))
          }
        }

        curClassName = tempName
        irs.append(MOV(scratchReg1, CP, DEFAULT))
        irs.append(POP(CP))

        irs.append(PUSH(scratchReg1))
        irs.toList
      }

      case StructLiter(exprs) => {
        val irs = ListBuffer.empty[IR]

        // malloc enough space for Struct
        val structName = lvalStructName(lval)
        val size = structTable.lookup(structName).get.structSize
        val saveParamRegs = willClobber(localRegs, liveMap)
        if (saveParamRegs) {
          irs.append(PUSHMul(paramRegs))
        }
        irs.append(MOVImm(R0, size, DEFAULT))
        irs.append(BRANCH("malloc", L))
        irs.append(MOV(R12, R0, DEFAULT))
        if (saveParamRegs) {
          irs.append(POPMul(paramRegs))
        }

        // R12 now holds the pointer to the structure at 0 offset
        // set all the expr in mem
        val structDefinition = structTable.lookup(structName).get
        val fields = structDefinition.getKeys()
        for (i <- exprs.indices) {
          irs.appendAll(generateExprIR(exprs(i), liveMap, localRegs))
          irs.append(POP(scratchReg1))
          irs.append(STR(scratchReg1, R12, structDefinition.getOffset(fields(i)).get, DEFAULT))
        }

        // push pointer to struct on stack
        irs.append(PUSH(R12))

        irs.toList
      }

      case ArrayLiter(exprs) => {
        val irs = ListBuffer.empty[IR]
        val exprLen = exprs.length
        val saveParamRegs = willClobber(localRegs, liveMap)

        val size = lvalCharArraySize(lval)

        if (saveParamRegs) {
          irs.append(PUSHMul(paramRegs))
        }
        irs.append(MOVImm(R0, (exprLen * size) + WORD_SIZE, DEFAULT))
        irs.append(BRANCH("malloc", L))
        irs.append(MOV(R12, R0, DEFAULT))

        if (saveParamRegs) {
          irs.append(POPMul(paramRegs))
        }

        irs.append(ADD(R12, R12, WORD_SIZE, DEFAULT))

        // store size of array
        irs.append(MOVImm(scratchReg1, exprLen, DEFAULT))
        irs.append(STR(scratchReg1, R12, -WORD_SIZE, DEFAULT))

        // set all the expr in mem
        for (i <- exprs.indices) {
          irs.appendAll(generateExprIR(exprs(i), liveMap, localRegs))
          irs.append(POP(scratchReg1))
          irs.append(STR(scratchReg1, R12, i * size, DEFAULT))
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
          irs.appendAll(generateExprIR(lArgs(i), liveMap, localRegs)) // this leaves the value on top of stack for function call
        }

        for (i <- 0 until math.min(paramRegs.length, lArgs.length)) {
          irs.append(POP(paramRegs(i)))
        }

        var prefix = FUNCTION_PREFIX
        if (curClassName.isDefined) {
          prefix = CLASS_METHOD_PREFIX + curClassName.get + "_"
        }

        irs.append(BRANCH(prefix + ident, L))

        if (lArgs.length > WORD_SIZE) {
          irs.append(ADD(SP, SP, (lArgs.length - WORD_SIZE) * WORD_SIZE, DEFAULT))
        }

        irs.append(MOV(scratchReg1, R0, DEFAULT))

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

      case mc@MethodCall(ident, methodName, args) => {
        val localCount = liveMap.getNestedEntries()
        val irs = ListBuffer.empty[IR]

        // setting up curr class pointer and method prefix
        val tempName = curClassName

        curClassName = mc.symbolTable.get.lookupAll(ident).get match {
          case ClassSemType(className) => Some(className)
          case _ => throw new RuntimeException("should not reach here")
        }
        irs.append(PUSH(CP)) // saving class pointer
        irs.appendAll(getIntoTarget(ident, CP, liveMap))

        // caller saved
        if (numParams > 0) {
          irs.append(PUSHMul(paramRegs.slice(0, numParams)))
        }
        val realCount = localCount - numParams
        if (realCount > 0) {
          irs.append(PUSHMul(localRegs.slice(0, realCount)))
        }

        // this leaves the value on top of stack for function call
        for (i <- args.length - 1 to 0 by -1) {
          irs.appendAll(generateExprIR(args(i), liveMap, localRegs))
        }

        for (i <- 0 until math.min(paramRegs.length, args.length)) {
          irs.append(POP(paramRegs(i)))
        }

        val prefix = CLASS_METHOD_PREFIX + curClassName.get + "_"
        irs.append(BRANCH(prefix + methodName, L))

        if (args.length > WORD_SIZE) {
          irs.append(ADD(SP, SP, (args.length - paramRegs.length) * WORD_SIZE, DEFAULT))
        }

        irs.append(MOV(scratchReg1, R0, DEFAULT))

        // caller saved
        if (realCount > 0) {
          irs.append(POPMul(localRegs.slice(0, realCount)))
        }
        if (numParams > 0) {
          irs.append(POPMul(paramRegs.slice(0, numParams)))
        }

        irs.append(POP(CP))
        curClassName = tempName

        irs.append(PUSH(scratchReg1))
        irs.toList
      }

    }
  }

  private def getArrElemType(arrElem: ArrayElem): SemType = {
    var arrType = arrElem.st.get.lookupAll(arrElem.ident).get
    var depth = arrElem.exprs.length

    while (depth > 0) {
      arrType match {
        case ArraySemType(t) => {
          arrType = t
        }
      }
      depth -= 1
    }
    arrType
  }

  private def arrayElemIsChar(arrElem: ArrayElem): Boolean = {
    getArrElemType(arrElem) match {
      case SemTypes.CharSemType => true
      case _ => false
    }
  }

  // we are working with caller saved strategy
  // Parameters stored in r0-r3 and then on stack
  private def generateFuncIR(func: Func, totalLocalRegs: List[Reg]): List[IR] = {
    val irs = ListBuffer.empty[IR]
    var prefix = FUNCTION_PREFIX
    if (curClassName.isDefined) {
      prefix = CLASS_METHOD_PREFIX + curClassName.get + "_"
    }
    irs += Label(prefix + func.ident)

    irs += PUSHMul(List(FP, LR))
    irs.append(MOV(FP, SP, DEFAULT))

    // set up function parameters
    val liveMap = new SymbolTable[Location](None)

    val numParams = func.params.length
    for (i <- 0 until numParams) {
      if (i < WORD_SIZE) {
        liveMap.add(func.params(i).ident, paramRegs(i))
      } else {
        liveMap.add(func.params(i).ident, Stack((i - 2) * WORD_SIZE))
      }
    }

    // setting up function local registers
    val localRegsBuilder = ListBuffer.empty[Reg]
    val totalNum = totalLocalRegs.length
    localRegsBuilder.appendAll(totalLocalRegs.slice(0, totalNum - paramRegs.length))
    if (numParams <= WORD_SIZE) {
      for (i <- (totalNum - (WORD_SIZE - numParams)) until totalNum) {
        localRegsBuilder.append(totalLocalRegs(i))
      }
    }

    // this code should consume the statement body of the function (Caller saved convention)
    val numLocalsInBody = findNumLocals(func.stat)
    val localRegs = localRegsBuilder.toList
    val numLocalRegs = localRegs.length
    if (numLocalsInBody > numLocalRegs) {
      irs.append(SUB(SP, SP, (numLocalsInBody - numLocalRegs) * WORD_SIZE))
    }
    val childLiveMap = new SymbolTable[Location](Some(liveMap))
    irs.appendAll(generateStatIR(func.stat, childLiveMap, localRegs, numParams))
    irs += POPMul(List(FP, PC))

    if (numLocalsInBody > numLocalRegs) {
      irs.append(ADD(SP, SP, (numLocalsInBody - numLocalRegs) * WORD_SIZE, DEFAULT))
    }
    irs.append(LOCALCOLLECT)
    irs.toList
  }

  private def getArrayElemIr(liveMap: SymbolTable[Location], arrayElem: ArrayElem, localRegs: List[Reg]): List[IR] = {
    val ident = arrayElem.ident
    val exprs = arrayElem.exprs
    val irs = ListBuffer.empty[IR]

    irs.append(PUSH(R3))

    irs.appendAll(getIntoTarget(ident, R3, liveMap))

    val isChar = arrayElemIsChar(arrayElem)

    for (expr <- exprs) {
      irs.appendAll(generateExprIR(expr, liveMap, localRegs))
      irs.append(POP(R10))
      if (isChar) {
        irs.append(BRANCH("_arrLoadB", L))
      }
      else {
        irs.append(BRANCH("_arrLoad", L))
      }
    }

    if (isChar) {
      widgets.add(arrLoadB)
    } else {
      widgets.add(arrLoad)
    }

    // Now R3 contains the required element --> which must be a pair
    irs.append(MOV(scratchReg1, R3, DEFAULT))
    irs.append(POP(R3))
    irs.toList
  }

  private def getIdentValueIr(liveMap: SymbolTable[Location], s: String): List[IR] = {
    val irs = ListBuffer.empty[IR]
    irs.appendAll(getIntoTarget(s, scratchReg1, liveMap))
    irs.append(CMPImm(scratchReg1, 0))
    irs.append(BRANCH("_errNull", LEQ))
    widgets.add(errNull)
    irs.toList
  }

  // pushes the pointer to the pair elem on top of the stack
  private def getIRForPairElem(elem: PairElem, liveMap: SymbolTable[Location], localRegs: List[Reg]): List[IR] = {
    val irs = ListBuffer.empty[IR]
    var isFst = false

    elem match {
      case Fst(lvalue) =>
        isFst = true
        lvalue match {
          case insideElem: PairElem => {
            irs.appendAll(getIRForPairElem(insideElem, liveMap, localRegs))

            irs.append(POP(scratchReg1))
            irs.append(CMPImm(scratchReg1, 0))
            irs.append(BRANCH("_errNull", LEQ))
            irs.append(LDR(scratchReg1, scratchReg1, 0, DEFAULT))

            irs.append(CMPImm(scratchReg1, 0))
            irs.append(BRANCH("_errNull", LEQ))
            irs.append(LDR(scratchReg1, scratchReg1, 0, DEFAULT))
          }

          case structElem@StructElem(ident, field) => {
            structElem.st.get.lookupAll(ident).get match {
              case StructSemType(structName) => {
                val structDefinition = structTable.lookup(structName).get
                irs.appendAll(getIntoTarget(ident, scratchReg2, liveMap))
                irs.append(LDR(scratchReg1, scratchReg2, structDefinition.getOffset(field).get, DEFAULT)) // must be a pointer to a pair
                irs.append(LDR(scratchReg1, scratchReg1, 0, DEFAULT))
              }
              case _ => throw new RuntimeException("should not reach here")
            }
          }

          case classElem@ClassElem(ident, member) => {
            irs.append(PUSH(CP))
            val tempName = curClassName

            curClassName = classElem.st.get.lookupAll(ident).get match {
              case ClassSemType(className) => Some(className)
              case _ => throw new RuntimeException("should not reach here")
            }

            irs.appendAll(getIntoTarget(ident, CP, liveMap)) // set class pointer
            irs.appendAll(getMemberIntoTarget(member, scratchReg1))
            curClassName = tempName
            irs.append(POP(CP))
            irs.append(LDR(scratchReg1, scratchReg1, 0, DEFAULT))
          }

          case arrElem: ArrayElem => {
            irs.appendAll(getArrayElemIr(liveMap, arrElem, localRegs))
            irs.append(LDR(scratchReg1, scratchReg1, 0, DEFAULT))
          }
          case IdentValue(s) => {
            irs.appendAll(getIdentValueIr(liveMap, s))
            irs.append(LDR(scratchReg1, scratchReg1, 0, DEFAULT))
          }
        }

      case Snd(lvalue) =>
        lvalue match {
          case insideElem: PairElem => {
            irs.appendAll(getIRForPairElem(insideElem, liveMap, localRegs))

            irs.append(POP(scratchReg1))
            irs.append(CMPImm(scratchReg1, 0)) // this loads the value... we want to load the pointer
            irs.append(BRANCH("_errNull", LEQ))
            irs.append(LDR(scratchReg1, scratchReg1, 0, DEFAULT))

            irs.append(CMPImm(scratchReg1, 0))
            irs.append(BRANCH("_errNull", LEQ))
            irs.append(LDR(scratchReg1, scratchReg1, WORD_SIZE, DEFAULT))
          }

          case structElem@StructElem(ident, field) => {
            structElem.st.get.lookupAll(ident).get match {
              case StructSemType(structName) => {
                val structDefinition = structTable.lookup(structName).get
                irs.appendAll(getIntoTarget(ident, scratchReg2, liveMap))
                irs.append(LDR(scratchReg1, scratchReg2, structDefinition.getOffset(field).get, DEFAULT)) // must be a pointer to a pair
                irs.append(LDR(scratchReg1, scratchReg1, WORD_SIZE, DEFAULT))
              }
              case _ => throw new RuntimeException("should not reach here")
            }
          }

          case classElem@ClassElem(ident, member) => {
            irs.append(PUSH(CP))
            val tempName = curClassName

            curClassName = classElem.st.get.lookupAll(ident).get match {
              case ClassSemType(className) => Some(className)
              case _ => throw new RuntimeException("should not reach here")
            }

            irs.appendAll(getIntoTarget(ident, CP, liveMap)) // set class pointer
            irs.appendAll(getMemberIntoTarget(member, scratchReg1))
            curClassName = tempName
            irs.append(POP(CP))
            irs.append(LDR(scratchReg1, scratchReg1, WORD_SIZE, DEFAULT))
          }

          case arrElem: ArrayElem => {
            irs.appendAll(getArrayElemIr(liveMap, arrElem, localRegs))
            irs.append(LDR(scratchReg1, scratchReg1, WORD_SIZE, DEFAULT))
          }
          case IdentValue(s) => {
            irs.appendAll(getIdentValueIr(liveMap, s))
            irs.append(LDR(scratchReg1, scratchReg1, WORD_SIZE, DEFAULT))
          }
        }
    }
    irs.append(PUSH(scratchReg1))
    widgets.add(errNull)
    irs.toList
  }

  private def arrElemIsPair(arrElem: ArrayElem): Boolean = {
    getArrElemType(arrElem) match {
      case _: PairSemType => true
      case _ => false
    }
  }

  private def generateStatIR(stat: Statement, liveMap: SymbolTable[Location], localRegs: List[Reg], numParams: Int): List[IR] = {
    stat match {
      case CallStat(ident, lArgs) => {
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
          irs.appendAll(generateExprIR(lArgs(i), liveMap, localRegs)) // this leaves the value on top of stack for function call
        }

        for (i <- 0 until math.min(WORD_SIZE, lArgs.length)) {
          irs.append(POP(paramRegs(i)))
        }

        irs.append(BRANCH(FUNCTION_PREFIX + ident, L))

        if (lArgs.length > WORD_SIZE) {
          irs.append(ADD(SP, SP, (lArgs.length - WORD_SIZE) * WORD_SIZE, DEFAULT))
        }

        // caller saved
        if (realCount > 0) {
          irs.append(POPMul(localRegs.slice(0, realCount)))
        }
        if (numParams > 0) {
          irs.append(POPMul(paramRegs.slice(0, numParams)))
        }

        irs.toList
      }


      case Exit(e) => generateExprIR(e, liveMap, localRegs) ++ List(POP(R0), BRANCH("exit", L))

      case Skip => List.empty[IR]

      case varDec@VarDec(_, ident, rvalue) => {
        val irs = ListBuffer.empty[IR]
        irs.appendAll(generateRvalue(rvalue, liveMap, localRegs, numParams, IdentValue(ident)(varDec.symbolTable, varDec.pos)))
        irs.appendAll(
          assignLocal(ident, liveMap, localRegs, numParams, varDec.symbolTable.get.lookupAll(ident).get == CharSemType))
        irs.toList
      }

      case Assign(lvalue, rvalue) => {
        lvalue match {
          case elem: PairElem => {
            val irs = ListBuffer.empty[IR]
            irs.appendAll(getIRForPairElem(elem, liveMap, localRegs))
            irs.appendAll(generateRvalue(rvalue, liveMap, localRegs, numParams, lvalue))
            irs.append(POP(scratchReg1))
            irs.append(POP(scratchReg2))
            //            val size = lvalCharArraySize(elem)
            irs.append(STR(scratchReg1, scratchReg2, 0, DEFAULT))
            irs.toList
          }

          case structElem@StructElem(ident, field) => {
            val irs = ListBuffer.empty[IR]

            structElem.st.get.lookupAll(ident).get match {
              case StructSemType(structName) => {
                val structDefinition = structTable.lookup(structName).get

                irs.appendAll(generateRvalue(rvalue, liveMap, localRegs, numParams, lvalue))
                irs.append(POP(scratchReg2))

                irs.appendAll(getIntoTarget(ident, scratchReg1, liveMap))
                if (structDefinition.lookup(field).get == CharSemType) {
                  irs.append(STR(scratchReg2, scratchReg1, structDefinition.getOffset(field).get, BYTECONSTOFFSET))
                } else {
                  irs.append(STR(scratchReg2, scratchReg1, structDefinition.getOffset(field).get, DEFAULT))
                }
              }
              case _ => throw new RuntimeException("should not reach here")
            }

            irs.toList
          }


          case classElem@ClassElem(ident, member) => {
            val irs = ListBuffer.empty[IR]

            classElem.st.get.lookupAll(ident).get match {
              case ClassSemType(className) => {
                val classDefn = classTable.lookup(className).get

                irs.appendAll(generateRvalue(rvalue, liveMap, localRegs, numParams, lvalue))
                irs.append(POP(scratchReg2))

                irs.append(PUSH(CP))
                val tempName = curClassName
                curClassName = Some(className)
                irs.appendAll(getIntoTarget(ident, CP, liveMap))
                val offset: Int = classDefn.getFieldOffset(member).get
                if (classDefn.getFieldType(member).get == CharSemType) {
                  irs.append(STR(scratchReg2, CP, offset, BYTECONSTOFFSET))
                } else {
                  irs.append(STR(scratchReg2, CP, offset, DEFAULT))
                }
                curClassName = tempName
                irs.append(POP(CP))
              }
              case _ => throw new RuntimeException("should not reach here")
            }
            irs.toList
          }


          case arrElem@ArrayElem(ident, exprs) => {
            val irs = ListBuffer.empty[IR]
            irs.append(PUSH(R3))
            irs.appendAll(getIntoTarget(ident, R3, liveMap))

            val isChar = arrayElemIsChar(arrElem)

            for (expr <- exprs.slice(0, exprs.length - 1)) {
              irs.appendAll(generateExprIR(expr, liveMap, localRegs))
              irs.append(POP(R10))
              if (isChar) {
                irs.append(BRANCH("_arrLoadB", L))
              }
              else {
                irs.append(BRANCH("_arrLoad", L))
              }
            }

            // Now R3 contains the required array and last expr is what we want
            irs.appendAll(generateExprIR(exprs.last, liveMap, localRegs))
            irs.append(POP(R10))
            irs.appendAll(generateRvalue(rvalue, liveMap, localRegs, numParams, lvalue))
            irs.append(POP(scratchReg1))

            if (isChar) {
              irs.append(BRANCH("_arrStoreB", L))
              widgets.add(arrStoreB)
            }
            else {
              irs.append(BRANCH("_arrStore", L))
              widgets.add(arrStore)
            }

            irs.append(POP(R3))
            irs.toList
          }
          case IdentValue(s) => {
            val irs = ListBuffer.empty[IR]
            irs.appendAll(generateRvalue(rvalue, liveMap, localRegs, numParams, lvalue))

            liveMap.lookupAll(s).get match {
              case reg: Reg => irs.append(POP(reg))
              case Stack(offset) => irs.appendAll(List(POP(scratchReg1), STR(scratchReg1, SP, offset, DEFAULT)))
            }
            irs.toList
          }
        }
      }


      case ConsecStat(first, next) => generateStatIR(first, liveMap, localRegs, numParams) ++ generateStatIR(next, liveMap, localRegs, numParams)

      case ScopeStat(stat) => generateStatIR(stat, new SymbolTable[Location](Some(liveMap)), localRegs, numParams)

      case mc@MethodStat(ident, methodName, args) => {
        val localCount = liveMap.getNestedEntries()
        val irs = ListBuffer.empty[IR]

        // setting up curr class pointer and method prefix
        val tempName = curClassName

        curClassName = mc.st.get.lookupAll(ident).get match {
          case ClassSemType(className) => Some(className)
          case _ => throw new RuntimeException("should not reach here")
        }
        irs.append(PUSH(CP)) // saving class pointer
        irs.appendAll(getIntoTarget(ident, CP, liveMap))

        // caller saved
        if (numParams > 0) {
          irs.append(PUSHMul(paramRegs.slice(0, numParams)))
        }
        val realCount = localCount - numParams
        if (realCount > 0) {
          irs.append(PUSHMul(localRegs.slice(0, realCount)))
        }

        // this leaves the value on top of stack for function call
        for (i <- args.length - 1 to 0 by -1) {
          irs.appendAll(generateExprIR(args(i), liveMap, localRegs))
        }

        for (i <- 0 until math.min(paramRegs.length, args.length)) {
          irs.append(POP(paramRegs(i)))
        }

        val prefix = CLASS_METHOD_PREFIX + curClassName.get + "_"
        irs.append(BRANCH(prefix + methodName, L))

        if (args.length > WORD_SIZE) {
          irs.append(ADD(SP, SP, (args.length - paramRegs.length) * WORD_SIZE, DEFAULT))
        }

        // caller saved
        if (realCount > 0) {
          irs.append(POPMul(localRegs.slice(0, realCount)))
        }
        if (numParams > 0) {
          irs.append(POPMul(paramRegs.slice(0, numParams)))
        }

        irs.append(POP(CP))
        curClassName = tempName
        irs.toList
      }

      case While(cond, doStat) => {
        val label1: String = getNewLabel
        val label2: String = getNewLabel

        val whileIr = ListBuffer.empty[IR]
        whileIr.append(BRANCH(label2, DEFAULT))
        whileIr.append(Label(label1))
        val doLiveMap = new SymbolTable[Location](Some(liveMap))
        whileIr.appendAll(generateStatIR(doStat, doLiveMap, localRegs, numParams))
        whileIr.append(Label(label2))
        whileIr.appendAll(generateExprIR(cond, liveMap, localRegs))
        whileIr.append(POP(scratchReg1))
        whileIr.append(CMPImm(scratchReg1, 0))
        whileIr.append(BRANCH(label1, NE))

        whileIr.toList
      }

      case If(cond, thenStat, elseStat) => {
        val label0: String = getNewLabel
        val label1: String = getNewLabel

        val ifIr = ListBuffer.empty[IR]
        ifIr.appendAll(generateExprIR(cond, liveMap, localRegs))
        ifIr.append(POP(scratchReg1))
        ifIr.append(CMPImm(scratchReg1, 0))
        ifIr.append(BRANCH(label1, EQ))
        val thenLiveMap = new SymbolTable[Location](Some(liveMap))
        ifIr.appendAll(generateStatIR(thenStat, thenLiveMap, localRegs, numParams))
        ifIr.append(BRANCH(label0, DEFAULT))
        ifIr.append(Label(label1))
        val elseLiveMap = new SymbolTable[Location](Some(liveMap))
        ifIr.appendAll(generateStatIR(elseStat, elseLiveMap, localRegs, numParams))
        ifIr.append(Label(label0))

        ifIr.toList
      }

      case Print(e, opType) => {
        getPrintIR(e, opType.get, liveMap, localRegs, ln = false)
      }

      case Println(e, opType) => {
        getPrintIR(e, opType.get, liveMap, localRegs, ln = true)
      }

      case Read(lvalue) => {
        lvalue match {
          case id@IdentValue(s) => {
            val idType = id.st.get.lookupAll(s)
            idType.get match {
              case SemTypes.IntSemType => getReadIr(s, isInt = true, liveMap, localRegs)
              case SemTypes.CharSemType => getReadIr(s, isInt = false, liveMap, localRegs)
            }
          }
          case elem: PairElem => {
            val irs = ListBuffer.empty[IR]

            val didClobber = willClobber(localRegs, liveMap)
            if (didClobber) {
              irs.append(PUSHMul(paramRegs))
            }

            // R0 should hold the value of ident
            irs.appendAll(getIRForPairElem(elem, liveMap, localRegs)) // still on stack
            irs.append(POP(R0))

            val typeOfPair = elem match {
              case f: Fst => f.ty
              case s: Snd => s.ty
            }

            typeOfPair match {
              case IntSemType => {
                widgets.add(readInt)
                irs.append(BRANCH("_readi", L))
              }
              case CharSemType => {
                widgets.add(readChar)
                irs.append(BRANCH("_readc", L))
              }
            }

            irs.append(PUSH(R0))
            irs.appendAll(getIRForPairElem(elem, liveMap, localRegs))
            irs.append(POP(scratchReg2))
            irs.append(POP(scratchReg1))
            irs.append(STR(scratchReg1, scratchReg2, 0, DEFAULT))

            if (didClobber) {
              irs.append(POPMul(paramRegs))
            }

            irs.toList
          }

          case structElem@StructElem(ident, field) => {
            val irs = ListBuffer.empty[IR]

            val structDefinition: StructTable.StructDef = structElem.st.get.lookupAll(ident).get match {
              case StructSemType(structName) => structTable.lookup(structName).get
              case _ => throw new RuntimeException("should not reach here")
            }

            val isChar = structDefinition.lookup(field).get == CharSemType

            val shouldSave = willClobber(localRegs, liveMap)
            if (shouldSave) {
              irs.append(PUSHMul(paramRegs))
            }

            irs.appendAll(getIntoTarget(ident, scratchReg1, liveMap))
            if (isChar) {
              irs.append(LDR(R0, scratchReg1, structDefinition.getOffset(field).get, BYTECONSTOFFSET))
            } else {
              irs.append(LDR(R0, scratchReg1, structDefinition.getOffset(field).get, DEFAULT))
            }

            // Now R0 contains old value for read
            if (isChar) {
              widgets.add(readChar)
              irs.append(BRANCH("_readc", L))
            } else {
              widgets.add(readInt)
              irs.append(BRANCH("_readi", L))
            }

            irs.appendAll(getIntoTarget(ident, scratchReg1, liveMap))

            if (isChar) {
              irs.append(STR(R0, scratchReg1, structDefinition.getOffset(field).get, BYTECONSTOFFSET))
            } else {
              irs.append(STR(R0, scratchReg1, structDefinition.getOffset(field).get, DEFAULT))
            }

            if (shouldSave) {
              irs.append(POPMul(paramRegs))
            }

            irs.toList

          }

          case classElem@ClassElem(ident, member) => {
            val irs = ListBuffer.empty[IR]

            val tempName = curClassName

            val classDefn = classElem.st.get.lookupAll(ident).get match {
              case ClassSemType(className) => {
                curClassName = Some(className)
                classTable.lookup(className).get
              }
              case _ => throw new RuntimeException("should not reach here")
            }

            irs.append(PUSH(CP)) // SAVING CLASS POINTER
            irs.appendAll(getIntoTarget(ident, CP, liveMap))

            val isChar = classDefn.getFieldType(member).get == CharSemType

            val shouldSave = willClobber(localRegs, liveMap)
            if (shouldSave) {
              irs.append(PUSHMul(paramRegs))
            }

            irs.appendAll(getMemberIntoTarget(member, R0))

            // Now R0 contains old value for read
            if (isChar) {
              widgets.add(readChar)
              irs.append(BRANCH("_readc", L))
            } else {
              widgets.add(readInt)
              irs.append(BRANCH("_readi", L))
            }

            //            irs.appendAll(getIntoTarget(ident, CP, liveMap))
            val offset = classDefn.getFieldOffset(member).get
            if (isChar) {
              irs.append(STR(R0, CP, offset, BYTECONSTOFFSET))
            } else {
              irs.append(STR(R0, CP, offset, DEFAULT))
            }

            if (shouldSave) {
              irs.append(POPMul(paramRegs))
            }

            curClassName = tempName
            irs.append(POP(CP))

            irs.toList
          }

          case ar@ArrayElem(ident, exprs) => {
            val irs = ListBuffer.empty[IR]

            val isChar = arrayElemIsChar(ar)

            irs.append(PUSH(R3))

            // place array on stack for first index
            irs.appendAll(getIntoTarget(ident, R3, liveMap))

            for (expr <- exprs) {
              irs.appendAll(generateExprIR(expr, liveMap, localRegs))
              irs.append(POP(R10))
              if (isChar) {
                irs.append(BRANCH("_arrLoadB", L))
              } else {
                irs.append(BRANCH("_arrLoad", L))
              }
            }

            if (isChar) {
              widgets.add(arrLoadB)
            } else {
              widgets.add(arrLoad)
            }


            // at this point R3 - contains the accessed value arr[x][y]
            val shouldSave = willClobber(localRegs, liveMap)
            if (shouldSave) {
              irs.append(PUSHMul(paramRegs.slice(0, 3)))
            }

            irs.append(MOV(R0, R3, DEFAULT))

            // find out what kind of read it is
            getArrType(ar.st.get.lookupAll(ident).get) match {
              case SemTypes.CharSemType => {
                widgets.add(readChar)
                irs.append(BRANCH("_readc", L))
              }
              case SemTypes.IntSemType => {
                widgets.add(readInt)
                irs.append(BRANCH("_readi", L))
              }
            }

            irs.append(MOV(R12, R0, DEFAULT)) // R12 holds the value of the read

            irs.appendAll(getIntoTarget(ident, R3, liveMap))
            for (expr <- exprs.slice(0, exprs.length - 1)) {
              irs.appendAll(generateExprIR(expr, liveMap, localRegs))
              irs.append(POP(R10))
              if (isChar) {
                irs.append(BRANCH("_arrLoadB", L))
              } else {
                irs.append(BRANCH("_arrLoad", L))
              }
            }

            // Now R3 contains the required array and last expr is what we want
            irs.appendAll(generateExprIR(exprs.last, liveMap, localRegs))
            irs.append(POP(R10))
            irs.append(MOV(scratchReg1, R12, DEFAULT))

            if (isChar) {
              irs.append(BRANCH("_arrStoreB", L))
              widgets.add(arrStoreB)
            } else {
              irs.append(BRANCH("_arrStore", L))
              widgets.add(arrStore)

            }

            if (shouldSave) {
              irs.append(POPMul(paramRegs.slice(0, 3)))
            }
            irs.append(POP(R3))

            irs.toList
          }
        }
      }
      case Free(e) => {
        e match {
          case id@IdentExpr(ident) => freeIdentWithGetIntoTargetIR(id.st.get, ident, getIntoTarget(ident, R0, liveMap), localRegs, liveMap)

          case th@ThisExpr(ident) => freeIdentWithGetIntoTargetIR(th.st.get, CLASS_FIELD_PREFIX + ident, getMemberIntoTarget(ident, R0), localRegs, liveMap)

          case structElem@StructElem(ident, field) => {
            val irs = ListBuffer.empty[IR]

            val saveParams = willClobber(localRegs, liveMap)
            if (saveParams) {
              irs.append(PUSHMul(paramRegs))
            }

            val structDefinition = structElem.st.get.lookupAll(ident).get match {
              case StructSemType(structName) => structTable.lookup(structName).get
              case _ => throw new RuntimeException("should not reach here")
            }

            // put in r0 the thing to be freed
            irs.appendAll(getIntoTarget(ident, scratchReg1, liveMap))
            irs.append(LDR(R0, scratchReg1, structDefinition.getOffset(field).get, DEFAULT))

            structDefinition.lookup(field).get match {
              case _: PairSemType => {
                irs.append(BRANCH("_freepair", L))
                widgets.add(freepair)
              }
              case _: ArraySemType => {
                irs.append(SUB(R0, R0, WORD_SIZE))
                irs.append(BRANCH("free", L))
              }

              case _ => {

                irs.append(BRANCH("free", L))
              }
            }

            if (saveParams) {
              irs.append(POPMul(paramRegs))
            }
            irs.toList
          }

          case arrElem@ArrayElem(ident, expressions) => {
            val irs = ListBuffer.empty[IR]
            irs.append(PUSH(R3)) // save R3

            // place array on stack for first index
            irs.appendAll(getIntoTarget(ident, R3, liveMap))

            for (expr <- expressions) {
              irs.appendAll(generateExprIR(expr, liveMap, localRegs))
              irs.append(POP(R10))
              irs.append(BRANCH("_arrLoad", L))
            }

            widgets.add(arrLoad)

            // handles the case when array elem is pair
            if (arrElemIsPair(arrElem)) {
              val saveParams = willClobber(localRegs, liveMap)
              if (saveParams) {
                irs.append(PUSHMul(paramRegs.slice(0, 3)))
              }

              irs.append(MOV(R0, R3, DEFAULT))

              irs.append(BRANCH("_freepair", L))
              widgets.add(freepair)

              if (saveParams) {
                irs.append(POPMul(paramRegs.slice(0, 3)))
              }
              return irs.toList
            }

            // right now r3 hold ths array
            irs.append(SUB(R3, R3, WORD_SIZE))

            val saveParams = willClobber(localRegs, liveMap)

            if (saveParams) {
              irs.append(PUSHMul(paramRegs))
            }

            irs.append(MOV(R0, R3, DEFAULT))
            irs.append(BRANCH("free", L))

            if (saveParams) {
              irs.append(POPMul(paramRegs))
            }

            irs.append(POP(R3)) // save R3
            irs.toList
          }

          case classElem@ClassElem(ident, member) => {
            val irs = ListBuffer.empty[IR]

            irs.append(PUSH(CP)) // save Class Pointer
            irs.appendAll(getIntoTarget(ident, CP, liveMap)) // put in r0 the thing to be freed

            val saveParams = willClobber(localRegs, liveMap)
            if (saveParams) {
              irs.append(PUSHMul(paramRegs))
            }

            val classDefn = classElem.st.get.lookupAll(ident).get match {
              case ClassSemType(className) => {
                curClassName = Some(className)
                classTable.lookup(className).get
              }
              case _ => throw new RuntimeException("should not reach here")
            }

            irs.appendAll(getMemberIntoTarget(member, R0))

            classDefn.getFieldType(member).get match {
              case _: PairSemType =>
                irs.append(BRANCH("_freepair", L))
                widgets.add(freepair)

              case _: ArraySemType =>
                irs.append(SUB(R0, R0, WORD_SIZE))
                irs.append(BRANCH("free", L))

              case _ =>
                irs.append(BRANCH("free", L))
            }

            if (saveParams) {
              irs.append(POPMul(paramRegs))
            }

            curClassName = None
            irs.append(POP(CP)) // saving Class Pointer
            irs.toList
          }

          case _ => List.empty[IR]
        }
      }

      case Return(e) => generateExprIR(e, liveMap, localRegs).appendedAll(List(POP(R0), POPMul(List(FP, PC))))
    }
  }

  private def freeIdentWithGetIntoTargetIR(st: GenericTable[SemType],
                                           ident: String,
                                           targetMoveIR: List[IR],
                                           localRegs: List[Reg],
                                           liveMap: SymbolTable[Location]): List[IR] = {
    val irs = ListBuffer.empty[IR]
    val stType = st.lookupAll(ident).get
    stType match {
      case _: ArraySemType => {
        val saveParams = willClobber(localRegs, liveMap)

        if (saveParams) {
          irs.append(PUSHMul(paramRegs))
        }

        irs.appendAll(targetMoveIR)
        irs.append(SUB(R0, R0, WORD_SIZE))
        irs.append(BRANCH("free", L))

        if (saveParams) {
          irs.append(POPMul(paramRegs))
        }
      }

      case _: PairSemType => {
        val saveParams = willClobber(localRegs, liveMap)
        val irs = ListBuffer.empty[IR]
        if (saveParams) {
          irs.append(PUSHMul(paramRegs))
        }

        irs.appendAll(targetMoveIR)
        irs.append(BRANCH("_freepair", L))
        widgets.add(freepair)

        if (saveParams) {
          irs.append(POPMul(paramRegs))
        }
      }

      // for structs and classes
      case _ => {
        val saveParams = willClobber(localRegs, liveMap)

        if (saveParams) {
          irs.append(PUSHMul(paramRegs))
        }

        irs.appendAll(targetMoveIR)
        irs.append(BRANCH("free", L))

        if (saveParams) {
          irs.append(POPMul(paramRegs))
        }
      }

    }
    irs.toList
  }


  private def getArrType(arrType: SemType): SemType = {
    arrType match {
      case ArraySemType(t) => getArrType(t)
      case x => x
    }
  }

  private def willClobber(localReg: List[Reg], liveMap: SymbolTable[Location]): Boolean = {
    var i = 0
    while (i < localReg.length && i < liveMap.getNestedEntries()) {
      if (localReg(i) == R0) {
        return true
      }
      i += 1
    }
    !localReg.contains(R0)
  }

  private def getPrintIR(e: Expr, expType: SemType, liveMap: SymbolTable[Location], localRegs: List[Reg], ln: Boolean): List[IR] = {
    val irs = ListBuffer.empty[IR]

    val clobber = willClobber(localRegs, liveMap)

    if (clobber) {
      irs.append(PUSHMul(paramRegs))
    }

    irs.appendAll(generateExprIR(e, liveMap, localRegs))
    irs.append(POP(R0))

    val flag = expType match {
      case CharSemType => {
        widgets.add(printChar)
        "c"
      }
      case IntSemType => {
        widgets.add(printInt)
        "i"
      }
      case StringSemType => {
        widgets.add(printString)
        "s"
      }
      case PairSemType(_, _) => {
        widgets.add(printPointer)
        "p"
      }
      case BoolSemType => {
        widgets.add(printBool)
        "b"
      }
      case ArraySemType(t) => {
        if (t == CharSemType) {
          widgets.add(printString)
          "s"
        } else {
          widgets.add(printPointer)
          "p"
        }
      }
      case _ => {
        widgets.add(printPointer)
        "p"
      }

    }

    irs.append(BRANCH("_print" + flag, L))

    if (ln) {
      irs.append(BRANCH("_println", L))
      widgets.add(printNewLine)
    }

    if (clobber) {
      irs.append(POPMul(paramRegs))
    }
    irs.toList
  }

  private def getReadIr(ident: String, isInt: Boolean, liveMap: SymbolTable[Location], localReg: List[Reg]): List[IR] = {
    val irs = ListBuffer.empty[IR]

    val didClobber = willClobber(localReg, liveMap)

    if (didClobber) {
      irs.append(PUSHMul(paramRegs))
    }

    // R0 should hold the value of ident
    liveMap.lookupAll(ident).get match {
      case Stack(offset) => irs.append(LDR(R0, FP, offset, DEFAULT))
      case reg: Reg => irs.append(MOV(R0, reg, DEFAULT))
    }

    if (isInt) {
      widgets.add(readInt)
      irs.append(BRANCH("_readi", L))
    } else {
      widgets.add(readChar)
      irs.append(BRANCH("_readc", L))

    }

    irs.append(MOV(scratchReg1, R0, DEFAULT))

    if (didClobber) {
      irs.append(POPMul(paramRegs))
    }

    liveMap.lookupAll(ident).get match {
      case Stack(offset) => irs.append(STR(scratchReg1, FP, offset, DEFAULT))
      case reg: Reg => irs.append(MOV(reg, scratchReg1, DEFAULT))
    }

    irs.toList
  }


  private def getIntoTarget(name: String, target: Reg, liveMap: SymbolTable[Location]): List[IR] = {
    val location = liveMap.lookupAll(name)
    location.get match {
      case src: Reg => List(MOV(target, src, DEFAULT))
      case Stack(offset) => List(LDR(scratchReg1, FP, offset, DEFAULT), MOV(target, scratchReg1, DEFAULT))
    }
  }

  private def assignLocal(ident: String, liveMap: SymbolTable[Location], localRegs: List[Reg],
                          numParams: Int, isChar: Boolean): List[IR] = {
    val localCount = liveMap.getNestedEntries()
    val realLocal = localCount - numParams
    if (realLocal < localRegs.size) {
      liveMap.add(ident, localRegs(realLocal))
      List(POP(localRegs(realLocal)))
    } else {
      val offset = (realLocal - localRegs.size + 1) * (-WORD_SIZE)
      liveMap.add(ident, Stack(offset))
      if (isChar) {
        List(POP(scratchReg1), STR(scratchReg1, FP, offset, BYTECONSTOFFSET))
      } else {
        List(POP(scratchReg1), STR(scratchReg1, FP, offset, DEFAULT))
      }

    }
  }

  private def getNewLabel: String = {
    val label: String = ".L" + labelOrder.toString
    labelOrder += 1
    label
  }

  private def inlineAllFunctions(irs: ListBuffer[IR], funcs: List[Func], localRegs: List[Reg]): ListBuffer[IR] = {
    val (inlinedFuncMap, regularFuncList): (mutable.Map[String, List[IR]], List[List[IR]]) = separateInlineFunctions(funcs, localRegs)
    val convertedIRs: Map[String, List[IR]] = convertToInline(inlinedFuncMap)
    for (funcIR <- regularFuncList) {
      irs.appendAll(funcIR)
    }
    ListBuffer.from(inlineFunctions(irs, convertedIRs))
  }

  private def separateInlineFunctions(funcs: List[Func], localRegs: List[Reg]): (mutable.Map[String, List[IR]], List[List[IR]]) = {
    val inlineFuncIRsMap: mutable.Map[String, List[IR]] = collection.mutable.Map.empty
    val normalFuncIRs: ListBuffer[List[IR]] = ListBuffer.empty

    for (func <- funcs) {
      val funcIR = generateFuncIR(func, localRegs)
      var inlinable: Boolean = (funcIR.length <= MAX_INLINE_LENGTH) && (!func.ident.startsWith("wacc_class_"))
      for (instruction <- funcIR) {
        instruction match {
          case BRANCH(label, L) =>
            inlinable &&= !label.startsWith(FUNCTION_PREFIX) || label.startsWith("wacc_class_")
          case _ =>
        }
      }

      if (inlinable) {
        funcIR.head match {
          case Label(label) => inlineFuncIRsMap.addOne(label, funcIR)
          case _ =>
        }
      } else normalFuncIRs.append(funcIR)
    }
    (inlineFuncIRsMap, normalFuncIRs.toList)

  }

  private def convertToInline(funcIRsMap: mutable.Map[String, List[IR]]): Map[String, List[IR]] = {
    val inlineableFuncIRsMap: mutable.Map[String, List[IR]] = collection.mutable.Map.empty

    for (funcIR <- funcIRsMap) {
      val inlinedFunc: ListBuffer[IR] = new mutable.ListBuffer[IR]
      val convFuncIr: List[IR] = funcIR._2.tail

      for (funcIR <- convFuncIr) {
        funcIR match {
          case MOV(FP, SP, DEFAULT) =>
          case PUSHMul(List(FP, LR)) => inlinedFunc.append(PUSH(FP))
          case POPMul(List(FP, PC)) =>
            inlinedFunc.append(POP(FP))
            inlinedFunc.append(BRANCH("LABEL PLACEHOLDER", DEFAULT))
          case LOCALCOLLECT => inlinedFunc.append(Label("LABEL PLACEHOLDER"))
          case stat: IR => inlinedFunc.append(stat)
        }
      }
      inlineableFuncIRsMap.addOne(funcIR._1, inlinedFunc.toList)
    }
    inlineableFuncIRsMap.toMap
  }

  private def replaceLabelPlaceHolders(funcIR: List[IR]): List[IR] = {
    val replacedIR: ListBuffer[IR] = new ListBuffer[IR]
    val newLabel = getNewLabel
    for (instruction <- funcIR) {
      instruction match {
        case BRANCH("LABEL PLACEHOLDER", DEFAULT) => {
          replacedIR.append(BRANCH(newLabel, DEFAULT))
        }
        case Label("LABEL PLACEHOLDER") => replacedIR.append(Label(newLabel))
        case instruction: IR => replacedIR.append(instruction)
      }
    }
    replacedIR.toList
  }


  private def inlineFunctions(ir: ListBuffer[IR], funcIRs: Map[String, List[IR]]): List[IR] = {
    val inlinedIR: ListBuffer[IR] = new ListBuffer[IR]
    for (instruction <- ir) {
      instruction match {
        case branchStat@BRANCH(label, L) => {
          if (funcIRs.contains(label)) {
            inlinedIR.appendAll(replaceLabelPlaceHolders(funcIRs(label)))
          }
          else inlinedIR.append(branchStat)
        }
        case instruction: IR => inlinedIR.append(instruction)
      }
    }
    inlinedIR.toList
  }

  private def optimisePeepHole(irs: ListBuffer[IR]): ListBuffer[IR] = {
    val newIRs = ListBuffer.empty[IR]
    var i = 0
    while (i < irs.length) {
      val ir = irs(i)
      val redundantMov = ir match {
        case MOV(rd, rs, _) => if (rd == rs) true else false
        case _ => false
      }

      if (redundantMov) i += 1 else {
        if (i < (irs.length - 1)) {
          ir match {
            case PUSH(reg1) =>
              val irNext = irs(i + 1)
              irNext match {
                case POP(reg2) =>
                  if (reg1 == reg2) i += 2
                  else {
                    newIRs.append(MOV(reg2, reg1, DEFAULT))
                    i += 2
                  }
                case _ =>
                  newIRs.append(ir)
                  i += 1
              }

            case MOV(reg1, reg2, _) =>
              val irNext = irs(i + 1)
              newIRs.append(ir)
              irNext match {
                case MOV(reg3, reg4, _) => {
                  if (reg1 == reg4 && reg2 == reg3) {
                    i += 2
                  } else {
                    i += 1
                  }
                }
                case _ =>
                  i += 1
              }

            case LDR(rd1, rs1, offset1, _) =>
              newIRs.append(ir)
              i += checkIfSameMemoryIR(rd1, rs1, offset1, irs(i + 1))

            case STR(rd1, rs1, offset1, _) =>
              newIRs.append(ir)
              i += checkIfSameMemoryIR(rd1, rs1, offset1, irs(i + 1))

            case _ =>
              newIRs.append(ir)
              i += 1
          }
        } else {
          newIRs.append(ir)
          i += 1
        }
      }

    }
    newIRs
  }

  private sealed trait Widget {
    def getIR(): List[IR]
  }

  private case object readChar extends Widget {
    override def getIR(): List[IR] = {
      val ir = ListBuffer.empty[IR]
      ir.append(Data(List(" %c"), stringNum))
      ir.append(Label("_readc"))
      ir.append(PUSH(LR))
      ir.append(STR(R0, SP, -4, DEFAULT))
      ir.append(SUB(SP, SP, 4))
      ir.append(MOV(R1, SP, DEFAULT))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("scanf", L))
      ir.append(LDR(R0, SP, 0, DEFAULT))
      ir.append(ADD(SP, SP, 4, DEFAULT))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object readInt extends Widget {
    override def getIR(): List[IR] = {
      val ir = ListBuffer.empty[IR]
      ir.append(Data(List("%d"), stringNum))
      ir.append(Label("_readi"))
      ir.append(PUSH(LR))
      ir.append(PUSH(R0))
      ir.append(MOV(R1, SP, DEFAULT))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("scanf", L))
      ir.append(LDR(R0, SP, 0, DEFAULT))
      ir.append(ADD(SP, SP, WORD_SIZE, DEFAULT))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object freepair extends Widget {
    widgets.add(errNull)

    override def getIR(): List[IR] = {
      val ir = new ListBuffer[IR]
      ir.append(Label("_freepair"))
      ir.append(PUSH(LR))
      ir.append(MOV(scratchReg1, R0, DEFAULT))
      ir.append(CMPImm(scratchReg1, 0))
      ir.append(BRANCH("_errNull", LEQ))
      ir.append(LDR(R0, scratchReg1, 0, DEFAULT))
      ir.append(BRANCH("free", L))
      ir.append(LDR(R0, scratchReg1, WORD_SIZE, DEFAULT))
      ir.append(BRANCH("free", L))
      ir.append(MOV(R0, scratchReg1, DEFAULT))
      ir.append(BRANCH("free", L))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object printInt extends Widget {
    override def getIR(): List[IR] = {
      val format: String = "%d"
      val ir = ListBuffer.empty[IR]
      ir.append(Data(List(format), stringNum))
      ir.append(Label("_printi"))
      ir.append(PUSH(LR))
      ir.append(MOV(R1, R0, DEFAULT))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("printf", L))
      ir.append(MOVImm(R0, 0, DEFAULT))
      ir.append(BRANCH("fflush", L))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object printBool extends Widget {
    override def getIR(): List[IR] = {
      val ir = ListBuffer.empty[IR]
      ir.append(Data(List("false", "true", "%.*s"), stringNum))
      ir.append(Label("_printb"))
      ir.append(PUSH(LR))
      ir.append(CMPImm(R0, 0))
      val label0 = getNewLabel
      val label1 = getNewLabel
      ir.append(BRANCH(label0, NE))
      ir.append(StringInit(R2, stringNum))
      ir.append(BRANCH(label1, DEFAULT))
      ir.append(Label(label0))
      ir.append(StringInit(R2, stringNum + 1))
      ir.append(Label(label1))
      ir.append(LDR(R1, R2, -WORD_SIZE, DEFAULT))
      ir.append(StringInit(R0, stringNum + 2))
      ir.append(BRANCH("printf", L))
      ir.append(MOVImm(R0, 0, DEFAULT))
      ir.append(BRANCH("fflush", L))
      ir.append(POP(PC))
      stringNum += 3
      ir.toList
    }
  }

  private case object printChar extends Widget {
    override def getIR(): List[IR] = {
      val format: String = "%c"
      val ir = ListBuffer.empty[IR]
      ir.append(Data(List(format), stringNum))
      ir.append(Label("_printc"))
      ir.append(PUSH(LR))
      ir.append(MOV(R1, R0, DEFAULT))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("printf", L))
      ir.append(MOVImm(R0, 0, DEFAULT))
      ir.append(BRANCH("fflush", L))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object printString extends Widget {
    override def getIR(): List[IR] = {
      val format = "%.*s"
      val ir = ListBuffer.empty[IR]
      ir.append(Data(List(format), stringNum))
      ir.append(Label("_prints"))
      ir.append(PUSH(LR))
      ir.append(MOV(R2, R0, DEFAULT))
      ir.append(LDR(R1, R0, -WORD_SIZE, DEFAULT))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("printf", L))
      ir.append(MOVImm(R0, 0, DEFAULT))
      ir.append(BRANCH("fflush", L))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object printPointer extends Widget {
    override def getIR(): List[IR] = {
      val ir = ListBuffer.empty[IR]
      ir.append(Data(List("%p"), stringNum))
      ir.append(Label("_printp"))
      ir.append(PUSH(LR))
      ir.append(MOV(R1, R0, DEFAULT))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("printf", L))
      ir.append(MOVImm(R0, 0, DEFAULT))
      ir.append(BRANCH("fflush", L))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object printNewLine extends Widget {
    override def getIR(): List[IR] = {
      val ir = ListBuffer.empty[IR]
      ir.append(Data(List(""), stringNum))
      ir.append(Label("_println"))
      ir.append(PUSH(LR))
      ir.append(StringInit(R0, stringNum))
      ir.append(BRANCH("puts", L))
      ir.append(MOVImm(R0, 0, DEFAULT))
      ir.append(BRANCH("fflush", L))
      ir.append(POP(PC))
      stringNum += 1
      ir.toList
    }
  }

  private case object errNull extends Widget {
    widgets.add(printString)

    override def getIR(): List[IR] = {
      val ir = new ListBuffer[IR]
      ir.append(Data(List("fatal error: null pair de-referenced or freed\n"), stringNum))
      ir.append(Label("_errNull"))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("_prints", L))
      ir.append(MOVImm(R0, 255, DEFAULT))
      ir.append(BRANCH("exit", L))
      ir.toList
    }
  }

  private case object errOverflow extends Widget {
    widgets.add(printString)

    override def getIR(): List[IR] = {
      val ir = new ListBuffer[IR]
      ir.append(Data(List("fatal error: integer overflow or underflow occurred\n"), stringNum))
      ir.append(Label("_errOverflow"))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("_prints", L))
      ir.append(MOVImm(R0, 255, DEFAULT))
      ir.append(BRANCH("exit", L))
      ir.toList
    }
  }

  private case object errDivZero extends Widget {
    widgets.add(printString)

    override def getIR(): List[IR] = {
      val ir = new ListBuffer[IR]
      ir.append(Data(List("fatal error: division or modulo by zero\n"), stringNum))
      ir.append(Label("_errDivZero"))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("_prints", L))
      ir.append(MOVImm(R0, 255, DEFAULT))
      ir.append(BRANCH("exit", L))
      ir.toList
    }
  }

  private case object arrLoad extends Widget {
    widgets.add(boundsCheck)

    override def getIR(): List[IR] = {
      val ir = new ListBuffer[IR]
      ir.append(Label("_arrLoad"))
      ir.append(PUSH(LR))
      ir.append(CMPImm(R10, 0))
      ir.append(MOV(R1, R10, LT))
      ir.append(BRANCH("_boundsCheck", LLT))
      ir.append(LDR(LR, R3, -WORD_SIZE, DEFAULT))
      ir.append(CMP(R10, LR))
      ir.append(MOV(R1, R10, GE))
      ir.append(BRANCH("_boundsCheck", LGE))
      ir.append(LDR(R3, R10, 2, INDEX))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object arrLoadB extends Widget {
    widgets.add(boundsCheck)

    override def getIR(): List[IR] = {
      val ir = new ListBuffer[IR]
      ir.append(Label("_arrLoadB"))
      ir.append(PUSH(LR))
      ir.append(CMPImm(R10, 0))
      ir.append(MOV(R1, R10, LT))
      ir.append(BRANCH("_boundsCheck", LLT))
      ir.append(LDR(LR, R3, -WORD_SIZE, DEFAULT))
      ir.append(CMP(R10, LR))
      ir.append(MOV(R1, R10, GE))
      ir.append(BRANCH("_boundsCheck", LGE))
      ir.append(LDR(R3, R10, 0, BYTEREGOFFSET))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object arrStore extends Widget {
    widgets.add(boundsCheck)

    override def getIR(): List[IR] = {
      val ir = new ListBuffer[IR]
      ir.append(Label("_arrStore"))
      ir.append(PUSH(LR))
      ir.append(CMPImm(R10, 0))
      ir.append(MOV(R1, R10, LT))
      ir.append(BRANCH("_boundsCheck", LLT))
      ir.append(LDR(LR, R3, -WORD_SIZE, DEFAULT))
      ir.append(CMP(R10, LR))
      ir.append(MOV(R1, R10, GE))
      ir.append(BRANCH("_boundsCheck", LGE))
      ir.append(STOREINDEX(scratchReg1, R3, R10, 2))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object arrStoreB extends Widget {
    widgets.add(boundsCheck)

    override def getIR(): List[IR] = {
      val ir = new ListBuffer[IR]
      ir.append(Label("_arrStoreB"))
      ir.append(PUSH(LR))
      ir.append(CMPImm(R10, 0))
      ir.append(MOV(R1, R10, LT))
      ir.append(BRANCH("_boundsCheck", LLT))
      ir.append(LDR(LR, R3, -WORD_SIZE, DEFAULT))
      ir.append(CMP(R10, LR))
      ir.append(MOV(R1, R10, GE))
      ir.append(BRANCH("_boundsCheck", LGE))
      ir.append(STOREINDEXB(scratchReg1, R3, R10))
      ir.append(POP(PC))
      ir.toList
    }
  }

  private case object boundsCheck extends Widget {
    override def getIR(): List[IR] = {
      val ir = new ListBuffer[IR]
      ir.append(Data(List("fatal error: array index %d out of bounds\n"), stringNum))
      ir.append(Label("_boundsCheck"))
      ir.append(StringInit(R0, stringNum))
      stringNum += 1
      ir.append(BRANCH("printf", L))
      ir.append(MOVImm(R0, 0, DEFAULT))
      ir.append(BRANCH("fflush", L))
      ir.append(MOVImm(R0, 255, DEFAULT))
      ir.append(BRANCH("exit", L))
      ir.toList
    }
  }

  private def checkIfSameMemoryIR(rd1: Registers.Reg, rs1: Registers.Reg, offset1: Int, irNext: IR): Int = {
    irNext match {
      case STR(rd2, rs2, offset2, _) => {
        if (rd1 == rd2 && rs1 == rs2 && offset1 == offset2) 2 // same instructions
        else 1
      }
      case LDR(rd2, rs2, offset2, _) => {
        if (rd1 == rd2 && rs1 == rs2 && offset1 == offset2) 2 // unnecessary assembly
        else 1
      }
      case _ => 1
    }
  }
}
