package wacc

import wacc.IR.IR
import wacc.Registers.Reg

object IR {

  sealed trait IR

  case class Data() extends IR
  case class Text() extends IR
  case class Global(globals : List[String]) extends IR

  case class Label(label : String) extends IR

  case class MOV(rd : Reg, rs : Reg) extends IR
  case class MOVImm(rd : Reg, i : Int) extends IR

  case class PUSH(regs : List[Reg]) extends IR
  case class POP(regs : List[Reg]) extends IR

  case class BL(label : String) extends IR
}

object IRstate {
  case class IRstate(irs: List[IR], strs : List[String])
}

object Registers {
  sealed trait Reg

  case object R0 extends Reg {override def toString = "r0"}
  case object R1 extends Reg {override def toString = "r1"}
  case object R2 extends Reg {override def toString = "r2"}
  case object R3 extends Reg {override def toString = "r3"}
  case object R4 extends Reg {override def toString = "r4"}
  case object R5 extends Reg {override def toString = "r5"}
  case object R6 extends Reg {override def toString = "r6"}
  case object R7 extends Reg {override def toString = "r7"}
  case object R8 extends Reg {override def toString = "r8"}
  case object R9 extends Reg {override def toString = "r9"}
  case object R10 extends Reg {override def toString = "r10"}
  case object R11 extends Reg {override def toString = "r11"}
  case object R12 extends Reg {override def toString = "r12"}
  case object R13 extends Reg {override def toString = "r13"}
  case object R14 extends Reg {override def toString = "r14"}
  case object R15 extends Reg {override def toString = "r15"}

  case object FP extends Reg {override def toString = "fp"}
  case object LR extends Reg {override def toString = "lr"}
  case object PC extends Reg {override def toString = "pc"}
}
