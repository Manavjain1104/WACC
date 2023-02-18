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

  private val strings: ListBuffer[String] = ListBuffer.empty[String]
  private var stringNum = 0

  private val widgets = collection.mutable.Map.empty[String, collection.mutable.Set[String]]

  /*
  TODO - Back End
  1) Return and frees
  2) Heaps - Arrays and pair --> len expressions
  3) Rvalue - just has expr right now
  4) checking for overflow, div by 0
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
    irs.append(MOV(FP, SP))
    val numLocalsInMain = findNumLocals(program.stat)
    if (numLocalsInMain > numLocalRegs) {
      irs.append(SUB(SP, SP, (numLocalsInMain - numLocalRegs) * 4))
    }
    val liveMap = new SymbolTable[Location](None)
    irs.appendAll(generateStatIR(program.stat, liveMap, localRegs))

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
      if (widgets.contains("printNewLine")) irs.appendAll(printNewLine())

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
        stringNum += 1
        strings += s
        List(StringInit(scratchReg1, stringNum - 1), PUSH(scratchReg1))
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
          case LenExpr(_) => null // TODO
        }
      }

      case expr: BinopExpr => {
        expr match {
          case AddExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), ADD(scratchReg1, scratchReg2, 0), PUSH(scratchReg1))  // TODO : check for overflow
          case SubExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), SUB(scratchReg1, scratchReg2, 0), PUSH(scratchReg1))  // TODO : check for overflow
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
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "LE"), MOVImm(scratchReg1,0, "GE"), PUSH(scratchReg1))
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
  def generateRvalue(rvalue: RValue, liveMap: SymbolTable[Location], localRegs : List[Reg]) : List[IR] = {
    rvalue match {
      case expr: Expr => generateExprIR(expr, liveMap)
      //      case NewPair(expr1, expr2) => [] TODO

      case Call(ident, lArgs) => {
        val localCount = liveMap.getNestedEntries()
        val irs = ListBuffer.empty[IR]

        if (localCount > 0) {
          irs.append(PUSHMul(localRegs.slice(0, localCount))) // caller saved
        }

        for (i <- lArgs.indices) {
          val expr = lArgs(i)
          irs.appendAll(generateExprIR(expr, liveMap)) // this leaves the value on top of stack for function call
          if (i < 4) {
            irs.append(POP(paramRegs(i)))
          }
        }

        irs.append(BL(FUNCTION_PREFIX + ident))

        if (lArgs.length > 4) {
          irs.append(ADD(SP, SP, (lArgs.length - 4) * 4))
        }

        irs.append(MOV(scratchReg1, R0))

        if (localCount > 0) {
          irs.append(POPMul(localRegs.slice(0, localCount))) // caller saved
        }

        irs.append(PUSH(scratchReg1))
        irs.toList
      }
    }
  }

  // we are working with caller saved strategy
  // Parameters stored in r0-r3 and then on stack
  def generateFuncIR(func : Func, totalLocalRegs : List[Reg]): List[IR] = {
    val irs = ListBuffer.empty[IR]
    irs += Label(FUNCTION_PREFIX + func.ident)
    irs += PUSHMul(List(FP, LR))
    irs.append(MOV(FP, SP))

    // set up function parameters
    val liveMap = new SymbolTable[Location](None)

    val numParams = func.params.length
    for (i <- 0 until numParams) {
      if (numParams < 4) {
        liveMap.add(func.params(i).ident, paramRegs(i))
      } else {
        liveMap.add(func.params(i).ident, Stack(((numParams - i) * 4)))
      }
    }

    // setting up function local registers
    val localRegsBuilder = ListBuffer.empty[Reg]
    val totalNum = totalLocalRegs.length
    localRegsBuilder.appendAll(totalLocalRegs.slice(0,4))
    if (numParams <= 4) {
      for (i <- (totalNum - (4 - numParams)) until totalNum) {
        localRegsBuilder.append(totalLocalRegs(i))
      }
    }

    // this code should consume the main statement body of the program (Caller saved convention)
    val numLocalsInBody = findNumLocals(func.stat)
    val localRegs = localRegsBuilder.toList
    val numLocalRegs = localRegs.length
    if (numLocalsInBody > numLocalRegs) {
      irs.append(SUB(SP, SP, (numLocalsInBody - numLocalRegs) * 4))
    }
    val childLiveMap = new SymbolTable[Location](Some(liveMap))
    irs.appendAll(generateStatIR(func.stat, childLiveMap, localRegs))

    irs += POPMul(List(FP, PC))

    if (numLocalsInBody > numLocalRegs) {
      irs.append(ADD(SP, SP, (numLocalsInBody - numLocalRegs) * 4))
    }
    irs.append(LTORG)
    irs.toList
  }

  def generateStatIR(stat: Statement, liveMap : SymbolTable[Location], localRegs : List[Reg]): List[IR] = {
    stat match {

      case Exit(e) => generateExprIR(e, liveMap) ++ List(POP(R0), BL("exit"))

      case Skip => List.empty[IR]

      case ScopeStat(stat) => generateStatIR(stat, liveMap, localRegs)

      case VarDec(_, ident, rvalue) => generateRvalue(rvalue, liveMap, localRegs) ++ assignLocal(ident, liveMap, localRegs)

      case ConsecStat(first, next) => generateStatIR(first, liveMap, localRegs) ++ generateStatIR(next, liveMap, localRegs)

      case ScopeStat(stat) => generateStatIR(stat, new SymbolTable[Location](Some(liveMap)), localRegs)

      case While(cond, doStat) => {
      val label1 : String = getNewLabel()
      val label2 : String = getNewLabel()

      val whileIr = ListBuffer.empty[IR]
      whileIr.append(BUC(label2))
      whileIr.append(Label(label1))
      val doLiveMap = new SymbolTable[Location](Some(liveMap))
      whileIr.appendAll(generateStatIR(doStat, doLiveMap, localRegs))
      whileIr.append(Label(label2))
      whileIr.appendAll(generateExprIR(cond, liveMap))
      whileIr.append(POP(scratchReg1))
      whileIr.append(CMPImm(scratchReg1, 0))
      whileIr.append(BNE(label1))

      whileIr.toList
    }

      case If(cond, thenStat, elseStat) => {
      val label0 : String = getNewLabel()
      val label1 : String = getNewLabel()

      val ifIr = ListBuffer.empty[IR]
      ifIr.appendAll(generateExprIR(cond, liveMap))
      ifIr.append(POP(scratchReg1))
      ifIr.append(CMPImm(scratchReg1, 0))
      ifIr.append(BEQ(label1))
      val thenLiveMap = new SymbolTable[Location](Some(liveMap))
      ifIr.appendAll(generateStatIR(thenStat, thenLiveMap, localRegs))
      ifIr.append(BUC(label0))
      ifIr.append(Label(label1))
      val elseLiveMap = new SymbolTable[Location](Some(liveMap))
      ifIr.appendAll(generateStatIR(elseStat, elseLiveMap, localRegs))
      ifIr.append(Label(label0))

      ifIr.toList
    }

      case Print(e, opType) => {
        assert(opType.isDefined, "Expr must have a type")
        getPrintIR(e, opType.get, liveMap, ln = false)
      }

      case Println(e, opType) => {
        assert(opType.isDefined, "Expr must have a type")
        getPrintIR(e, opType.get, liveMap, ln = true)
      }

      case Return(e) => generateExprIR(e, liveMap).appendedAll(List(POP(R0), POPMul(List(FP, PC))))

      case _ => null // TODO
    }
  }

  def getPrintIR(e : Expr, expType : SemType, liveMap : SymbolTable[Location], ln : Boolean) = {
    val irs = ListBuffer.empty[IR]

    val clobber = liveMap.getNestedEntries() > 4
    if (clobber) {
      if (expType == BoolSemType || expType == StringSemType || expType == ArraySemType(CharSemType)) {
        irs.append(PUSHMul(List(R0, R1, R2)))
      } else {
        irs.append(PUSHMul(List(R0, R1)))
      }
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
        "n,;'/ot possible"
      }

    }

    irs.append(BL("_print" + flag))
    if (widgets.contains("print")) {
      widgets("print").add(flag)
    } else {
      widgets("print") = collection.mutable.Set(flag)
    }

    if (ln) {
      irs.append(BL("_println"))
      widgets("printNewLine") = collection.mutable.Set.empty
    }

    if (clobber) {
      if (expType == BoolSemType || expType == StringSemType || expType == ArraySemType(CharSemType)) {
        irs.append(POPMul(List(R0, R1, R2)))
      } else {
        irs.append(POPMul(List(R0, R1)))
      }
    }
    irs.toList
  }

  def printAll(flag : String) : List[IR] = {
    flag match {
      case "c" => printBasic("c")
      case "i" => printBasic("i")
      case "s" => printString()
      case "b" => printBool()
      case  _  => null
    }
  }

  private def printBool() : List[IR] = {
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List("false", "true", "%.*s"), stringNum))
    ir.append(Label("_printb"))
    ir.append(PUSH(LR))
    ir.append(CMPImm(R0, 0))
    val label0 = getNewLabel()
    val label1 = getNewLabel()
    ir.append(BNE(label0))
    ir.append(StringInit(R2, stringNum))
    ir.append(BUC(label1))
    ir.append(Label(label0))
    ir.append(StringInit(R2, stringNum + 1))
    ir.append(Label(label1))
    ir.append(LDR(R1, R2, -4))
    ir.append(StringInit(R0, stringNum + 2))
    ir.append(BL("printf"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BL("fflush"))
    ir.append(POP(PC))
    stringNum += 3
    ir.toList
  }


  private def printString() : List[IR] = {
    val flag = "s"
    val format = "%.*s"
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List(format), stringNum))
    ir.append(Label("_print" + flag))
    ir.append(PUSH(LR))
    ir.append(MOV(R2, R0))
    ir.append(LDR(R1, R0, -4))
    ir.append(StringInit(R0, stringNum))
    stringNum += 1
    ir.append(BL("printf"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BL("fflush"))
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
    ir.append(MOV(R1, R0))
    ir.append(StringInit(R0, stringNum))
    stringNum += 1
    ir.append(BL("printf"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BL("fflush"))
    ir.append(POP(PC))
    ir.toList
  }

  def printNewLine() : List[IR] = {
    val ir = ListBuffer.empty[IR]
    ir.append(Data(List(""), stringNum))
    ir.append(Label("_println"))
    ir.append(PUSH(LR))
    ir.append(StringInit(R0, stringNum))
    ir.append(BL("puts"))
    ir.append(MOVImm(R0, 0, "Default"))
    ir.append(BL("fflush"))
    ir.append(POP(PC))
    stringNum += 1
    ir.toList
  }

  def getIntoTarget(name: String, target : Reg, liveMap : SymbolTable[Location]): List[IR] = {
    val location = liveMap.lookupAll(name)
    assert(location.isDefined, name + " must be defined in live map for getting its val")
    location.get match {
      case src: Reg => List(MOV(target, src))
      case Stack(offset) => List(LDR(scratchReg1, FP, offset), MOV(target, scratchReg1))
    }
  }

  def assignLocal(ident: String, liveMap: SymbolTable[Location], localRegs : List[Reg]): List[IR] = {
    val localCount = liveMap.getNestedEntries() + 1
    assert(liveMap.lookup(ident).isEmpty, "First assignment of " + ident + " in child scope")
    if (localCount <= localRegs.size) {
      liveMap.add(ident, localRegs(localCount - 1))
      List(POP(localRegs(localCount - 1)))
    } else {
      val offset = (localCount - localRegs.size)*(-4)
      liveMap.add(ident, Stack(offset))
      List(POP(scratchReg1), STR(scratchReg1, FP, offset))
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
                  newIRs.append(MOV(reg2, reg1))
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

//  def getStringsData(): List[String] = strings.toList

}