package wacc

import wacc.AST._
import wacc.IR._
import wacc.Registers._

import scala.collection.mutable.ListBuffer

class codeGenerator {

  private final val BOUND_EXIT_CODE = 256

  val allRegs = List(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)

  def generateProgIR(program: Program): List[IR] = {
    val irs = ListBuffer.empty[IR]

    // assembly hygiene
    irs += Data()
    irs += Text()
    irs += Global(List("main"))

    // this is consuming the main body of the program
    irs += Label("main")
    irs += PUSH(List(FP, LR))
    irs.appendAll(generateStatIR(program.stat, allRegs))
    irs += POP(List(FP, PC))
    println(irs.toList)
    irs.toList
  }

  def generateExprIR(e: Expr, regs: List[Reg]): List[IR] = {
    e match {
      case IntExpr(x) => List(MOVImm(regs.head, x))
      case BoolExpr(b) => {
        if (b) List(MOVImm(regs.head, 1))
        else List(MOVImm(regs.head, 0))
      }
      case CharExpr(c) => {
        List(MOVImm(regs.head, c.toInt))
      }
    }
  }

  def generateStatIR(stat: Statement, regs: List[Reg]): List[IR] = {
    stat match {
      case Exit(e) => {
        generateExprIR(e, regs).appended(BL("exit"))
      }
      case AST.Skip => List.empty[IR]
    }
  }

}
