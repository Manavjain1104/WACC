package wacc

import wacc.AST._
import wacc.IR._
import wacc.Registers._

import scala.collection.mutable.ListBuffer

class codeGenerator(program: Program) {

  private val localRegs: List[Reg] = List(R4, R5, R6, R7, R0, R1, R2, R3)
  private val NUMLOCALREGS = localRegs.length
  private var localCount = 0
  private var labelOrder = 0

  val strings: ListBuffer[String] = ListBuffer.empty[String]
  var stringNum: Int = 0

  /*
  TODO - Back End
  1) Function assembly
  2) function calls
  3) Print, Println statements etc
  4) Return and frees
  5) Heaps - Arrays and pair --> len exprs
  6) Rvalue - just has expr right now
  7) checking for overflow, div by 0
   */

  def generateProgIR(): List[IR] = {
    val irs = ListBuffer.empty[IR]

    // assembly hygiene
    irs += Data()
    irs += Text()
    irs += Global(List("main"))

    // this is consuming the main body of the program
    irs += Label("main")
    irs += PUSHMul(List(FP, LR))
    irs.append(MOV(FP, SP))
    val numLocalsInMain = findNumLocals(program.stat)
    if (numLocalsInMain > NUMLOCALREGS) {
      irs.append(SUB(SP, SP, (numLocalsInMain - NUMLOCALREGS) * 4))
    }
    val liveMap = new SymbolTable[Location](None)
    irs.appendAll(generateStatIR(program.stat, liveMap))

    irs += MOVImm(R0, 0, "Default")
    irs += POPMul(List(FP, PC))
    irs.toList
  }

  // this function writes instructions that calculate the value of the expression
  // and leave them on top of the stack, only use R8 and R9 as temporaries
  def generateExprIR(expr: Expr, liveMap : SymbolTable[Location]): List[IR] = {
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
          case LenExpr(e) => null // TODO
        }
      }

      case expr: BinopExpr => {
        expr match {
          case AddExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), ADD(scratchReg1, scratchReg2, 0), PUSH(scratchReg1))  // TODO : check for overflow
          case SubExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), SUB(scratchReg1, scratchReg2, 0), PUSH(scratchReg1))  // TODO : check for overflow
          case MulExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), MUL(scratchReg1, scratchReg2), PUSH(scratchReg1)) // TODO : check for overflow
          case DivExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg2), POP(scratchReg1), DIV(scratchReg1, scratchReg2)) // TODO : check for division by zero
          // MOV(R0, R8), MOV(R1, R9), BL("__aeabi_idivmod")
          case ModExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), MOD(scratchReg1, scratchReg2))
          // MOV(R0, R8), MOV(R1, R9), BL("__aeabi_idivmod")
          case GTExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "GT"), MOVImm(scratchReg1,0, "LE"), PUSH(scratchReg1))
          case LTExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "LT"), MOVImm(scratchReg1,0, "GE"), PUSH(scratchReg1))
          case GTEQExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "GE"), MOVImm(scratchReg1,0, "LT"), PUSH(scratchReg1))
          case LTEQExpr(e1, e2) =>
            generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "LE"), MOVImm(scratchReg1,0, "GE"), PUSH(scratchReg1))
          case AndExpr(e1, e2) => {
            val L1 : List[IR] = generateExprIR(e1, liveMap)
            val L2 : List[IR] = generateExprIR(e2, liveMap)
            val label : String = getNewLabel()
            val L3 : List[IR] = List(POP(scratchReg1), POP(scratchReg2), AND(scratchReg1, scratchReg2, label), PUSH(scratchReg1))
            L1 ++ L2 ++ L3
          }
          case OrExpr(e1, e2) =>
            val L1 : List[IR] = generateExprIR(e1, liveMap)
            val L2 : List[IR] = generateExprIR(e2, liveMap)
            val label : String = getNewLabel()
            val L3 : List[IR] = List(POP(scratchReg1), POP(scratchReg2), OR(scratchReg1, scratchReg2, label), PUSH(scratchReg1))
            L1 ++ L2 ++ L3

          case EQExpr(e1, e2) =>  generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "EQ"), MOVImm(scratchReg1,0, "NE"), PUSH(scratchReg1))
          case NEQExpr(e1, e2) => generateExprIR(e1, liveMap) ++ generateExprIR(e2, liveMap) ++ List(POP(scratchReg1), POP(scratchReg2), CMP(scratchReg1, scratchReg2), MOVImm(scratchReg1, 1, "NE"), MOVImm(scratchReg1,0, "EQ"), PUSH(scratchReg1))
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
  def generateRvalue(rvalue: RValue, liveMap: SymbolTable[Location]) : List[IR] = {
    rvalue match {
      case expr: Expr => generateExprIR(expr, liveMap)
      //      case NewPair(expr1, expr2) => [] TODO
      //      case Call(ident, lArgs) => TODO
    }
  }

  def generateStatIR(stat: Statement, liveMap : SymbolTable[Location]): List[IR] = stat match {
    case Exit(e) => {
      generateExprIR(e, liveMap) ++ List(POP(R0), BL("exit"))
    }
    case Skip => List.empty[IR]

    case ScopeStat(stat) => generateStatIR(stat, liveMap)
    case VarDec(_, ident, rvalue) => generateRvalue(rvalue, liveMap) ++ assignLocal(ident, liveMap)
    case ConsecStat(first, next) => generateStatIR(first, liveMap) ++ generateStatIR(next, liveMap)
    case ScopeStat(stat) => {
      generateStatIR(stat, new SymbolTable[Location](Some(liveMap)))
    }

    case While(cond, doStat) => {
      val label1 : String = getNewLabel()
      val label2 : String = getNewLabel()

      val whileIr = ListBuffer.empty[IR]
      whileIr.append(BUC(label2))
      whileIr.append(Label(label1))
      val doLiveMap = new SymbolTable[Location](Some(liveMap))
      whileIr.appendAll(generateStatIR(doStat, doLiveMap))
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
      ifIr.appendAll(generateStatIR(thenStat, thenLiveMap))
      ifIr.append(BUC(label0))
      ifIr.append(Label(label1))
      val elseLiveMap = new SymbolTable[Location](Some(liveMap))
      ifIr.appendAll(generateStatIR(elseStat, elseLiveMap))
      ifIr.append(Label(label0))

      ifIr.toList
    }
    case _ => null // TODO
  }

  def getIntoTarget(name: String, target : Reg, liveMap : SymbolTable[Location]): List[IR] = {
    val location = liveMap.lookupAll(name)
    assert(location.isDefined, name + " must be defined in live map for getting its val")
    location.get match {
      case src: Reg => List(MOV(target, src))
      case Stack(offset) => List(LDR(scratchReg1, FP, offset), MOV(target, scratchReg1))
    }
  }

  def assignLocal(ident: String, liveMap: SymbolTable[Location]): List[IR] = {
    localCount += 1
    assert(liveMap.lookup(ident).isEmpty, "First assignment of " + ident + " in child scope")
    if (localCount <= NUMLOCALREGS) {
      liveMap.add(ident, localRegs(localCount - 1))
      List(POP(localRegs(localCount - 1)))
    } else {
      val offset = (localCount - NUMLOCALREGS)*(-4)
      liveMap.add(ident, Stack(offset))
      List(POP(scratchReg1), STR(scratchReg1, FP, offset))
    }
  }

  def getNewLabel(): String = {
    val label : String = ".L" + labelOrder.toString
    labelOrder += 1
    label
  }

  def numLocals() : Int = localCount

  def clearLiveMap(liveMap: SymbolTable[Location]) : Unit = liveMap.map.clear()

  def getStringsData(): List[String] = strings.toList

}