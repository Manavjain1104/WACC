package wacc

import wacc.AST._
import wacc.IR._
import wacc.Registers._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class codeGenerator(program: Program) {

  private val liveMap = mutable.Map.empty[String, Location]

  private val localRegs: List[Reg] = List(R4, R5, R6, R7, R0, R1, R2, R3, R11, R12, R13, R14, R15) // TODO : ask Jamie why he does 8 and is that related to the r4 r8.. push at the start of main
  private val NUMLOCALREGS = localRegs.length
  private var localCount = 0
  private var labelOrder = 0

  val strings: ListBuffer[String] = ListBuffer.empty[String]
  var stringNum: Int = 0

  /*
  TODO
  1) Function assembly
  2) function calls
  3) Heaps - Arrays and pair --> len exprs
  4) Stat - WHile, IF
  5) Rvalue - just has expr right now
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
      irs.append(SUB(SP, SP, (numLocalsInMain - NUMLOCALREGS) * 4)))
    }
    irs.appendAll(generateStatIR(program.stat))

    irs += MOVImm(R0, 0, "Default")
    irs += POPMul(List(FP, PC))
    //    println(irs.toList)
    irs.toList
  }

  // this function writes instructions that calculate the value of the expression
  // and leave them on top of the stack, only use R8 and R9 as temporaries
  def generateExprIR(expr: Expr): List[IR] = {
    expr match {
      case IntExpr(x) => List(MOVImm(R8, x, "Default"), PUSH(R8)) // TODO if we have a PUSH R8 followed by POP R8 remove that during printing
      case BoolExpr(b) => {
        var value = 0
        if (b) value = 1
        List(MOVImm(R8, value, "Default"), PUSH(R8))
      }
      case CharExpr(c) => {
        List(MOVImm(R8, c.toInt, "Default"), PUSH(R8))
      }
      case StringExpr(s) => {
        stringNum += 1
        strings += s
        List(StringInit(R8, stringNum - 1), PUSH(R8))
      }
      case IdentExpr(ident) => {
        // transfer identifier expr to the first available register
        getIntoTarget(ident, R8).appended(PUSH(R8))
      }

      case expr: UnopExpr => {
        expr match {
          case ChrExpr(e) => generateExprIR(e)
          case OrdExpr(e) => generateExprIR(e)
          case NegExpr(e) => generateExprIR(e) ++ List(POP(R8), NEG(R8, R8), PUSH(R8))
          case NotExpr(e) => generateExprIR(e) ++ List(POP(R8), NOT(R8, R8), PUSH(R8))
          case LenExpr(e) => null // TODO
        }
      }

      case expr: BinopExpr => {
        expr match {
          case AddExpr(e1, e2) =>
            generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), ADD(R8, R9, 0), PUSH(R8))  // TODO : check for overflow
          case SubExpr(e1, e2) =>
            generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), SUB(R8, R9, 0), PUSH(R8))  // TODO : check for overflow
          case MulExpr(e1, e2) =>
            generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), MUL(R8, R9), PUSH(R8)) // TODO : check for overflow
          case DivExpr(e1, e2) =>
            generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), DIV(R8, R9), PUSH(R8)) // TODO : check for division by zero
          // MOV(R0, R8), MOV(R1, R9), BL("__aeabi_idivmod")
          case ModExpr(e1, e2) =>
            generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), MOD(R8, R9), PUSH(R8))
            // MOV(R0, R8), MOV(R1, R9), BL("__aeabi_idivmod")
          case GTExpr(e1, e2) =>
              generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), CMP(R8, R9), MOVImm(R8, 1, "GT"), MOVImm(R8,0, "LE"), PUSH(R8))
          case LTExpr(e1, e2) =>
            generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), CMP(R8, R9), MOVImm(R8, 1, "LT"), MOVImm(R8,0, "GE"), PUSH(R8))
          case GTEQExpr(e1, e2) =>
            generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), CMP(R8, R9), MOVImm(R8, 1, "GE"), MOVImm(R8,0, "LT"), PUSH(R8))
          case LTEQExpr(e1, e2) =>
            generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), CMP(R8, R9), MOVImm(R8, 1, "LE"), MOVImm(R8,0, "GE"), PUSH(R8))
          case AndExpr(e1, e2) => {
            val L1 : List[IR] = generateExprIR(e1)
            val L2 : List[IR] = generateExprIR(e2)
            val label : String = getNewLabel()
            val L3 : List[IR] = List(POP(R8), POP(R9), AND(R8, R9, label), PUSH(R8))
            L1 ++ L2 ++ L3
          }
          case OrExpr(e1, e2) =>
            val L1 : List[IR] = generateExprIR(e1)
            val L2 : List[IR] = generateExprIR(e2)
            val label : String = getNewLabel()
            val L3 : List[IR] = List(POP(R8), POP(R9), OR(R8, R9, label), PUSH(R8))
            L1 ++ L2 ++ L3

          case EQExpr(e1, e2) =>  generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), CMP(R8, R9), MOVImm(R8, 1, "EQ"), MOVImm(R8,0, "NE"), PUSH(R8))
          case NEQExpr(e1, e2) => generateExprIR(e1) ++ generateExprIR(e2) ++ List(POP(R8), POP(R9), CMP(R8, R9), MOVImm(R8, 1, "NE"), MOVImm(R8,0, "EQ"), PUSH(R8))
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
  def generateRvalue(rvalue: RValue): List[IR] = {
    rvalue match {
      case expr: Expr => generateExprIR(expr)
      //      case NewPair(expr1, expr2) => [] TODO
      //      case Call(ident, lArgs) => TODO
    }
  }

  def generateStatIR(stat: Statement): List[IR] = stat match {
    case Exit(e) => {
      generateExprIR(e) ++ List(POP(R0), BL("exit"))
    }
    case Skip => List.empty[IR]

    case ScopeStat(stat) => generateStatIR(stat)
    case VarDec(_, ident, rvalue) => generateRvalue(rvalue) ++ assignLocal(ident)
    case ConsecStat(first, next) => generateStatIR(first) ++ generateStatIR(next)
    case ScopeStat(stat) => {
        val numSaved = localCount
        PUSHMul(localRegs.slice(0, numSaved)) :: generateStatIR(stat).appended(POPMul(localRegs.slice(0, numSaved)))
    }

    case While(cond, doStat) => {
      val label1 : String = getNewLabel()
      val label2 : String = getNewLabel()

      val numSaved = localCount

      val whileIr = ListBuffer.empty[IR]
      whileIr.append(PUSHMul(localRegs.slice(0, numSaved)))
      whileIr.append(BUC(label2))
      whileIr.append(Label(label1))
      whileIr.appendAll(generateStatIR(doStat))
      whileIr.append(Label(label2))
      whileIr.appendAll(generateExprIR(cond))
      whileIr.append(POP(R8))
      whileIr.append(CMPImm(R8, 0))
      whileIr.append(BNE(label1))
      whileIr.append(POPMul(localRegs.slice(0, numSaved)))

      whileIr.toList
    }

    case If(cond, thenStat, elseStat) => {
      val label0 : String = getNewLabel()
      val label1 : String = getNewLabel()

      val numSaved = localCount

      val ifIr = ListBuffer.empty[IR]
      ifIr.append(PUSHMul(localRegs.slice(0, numSaved)))
      ifIr.appendAll(generateExprIR(cond))
      ifIr.append(POP(R8))
      ifIr.append(CMPImm(R8, 0))
      ifIr.append(BEQ(label1))
      ifIr.appendAll(generateStatIR(thenStat))
      ifIr.append(BUC(label0))
      ifIr.append(Label(label1))
      ifIr.appendAll(generateStatIR(elseStat))
      ifIr.append(Label(label0))
      ifIr.append(POPMul(localRegs.slice(0, numSaved)))

      ifIr.toList
    }
    case _ => null // TODO
  }

  def getIntoTarget(name: String, target : Reg): List[IR] = {
    assert(liveMap.contains(name), name + " must be defined in live map for getting its val")
    liveMap.get(name).get match {
      case src: Reg => List(MOV(target, src))
      case Stack(offset) => List(LDR(R8, FP, offset), MOV(target, R8))
    }
  }

  def assignLocal(ident: String): List[IR] = {
    localCount += 1
    if (localCount <= NUMLOCALREGS) {
      liveMap(ident) = localRegs(localCount - 1)
      List(POP(localRegs(localCount - 1)))
    } else {
      val offset = (localCount - NUMLOCALREGS)*(-4)
      liveMap(ident) = Stack(offset)
      List(POP(R8), STR(R8, FP, offset))
    }
  }

  def getNewLabel(): String = {
    val label : String = ".L" + labelOrder.toString
    labelOrder += 1
    label
  }

  def clearLiveMap() = liveMap.clear()

  def getStringsData(): List[String] = strings.toList

}
