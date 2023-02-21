package wacc

import wacc.Registers.Reg

object IR {
  sealed trait IR

  // Top level statements
  case class Data(strings : List[String], startIndex : Int) extends IR
  case class Global(globals: List[String]) extends IR
  case object LTORG                        extends IR

  // Label and Branch Statements
  case class Label(label: String)  extends IR
  case class BRANCH(label: String, Suffix: String) extends IR


  // Move statements
  case class MOV(rd: Reg, rs: Reg, Flag : String)     extends IR
  case class MOVImm(rd: Reg, i: Int, Suffix : String) extends IR

  // Push and Pop Statements
  case class PUSHMul(regs: List[Reg]) extends IR
  case class PUSH(reg: Reg)           extends IR
  case class POPMul(regs: List[Reg])  extends IR
  case class POP(reg: Reg)            extends IR

  // Unary Operators
  case class NEG(rd : Reg, rs : Reg) extends IR
  case class NOT(rd : Reg, rs : Reg) extends IR

  // Arithmetic Binary Operators
  case class ADD(rd: Reg, rn: Reg, i: Int)                extends IR
  case class ADDREG(rd: Reg, rn: Reg, rm: Reg)            extends IR
  case class SUB(rd: Reg, rn: Reg, i: Int )               extends IR
  case class SUBREG(rd: Reg, rn: Reg, rm: Reg)            extends IR
  case class DIV(rd : Reg, rs : Reg, locals : Int)        extends IR
  case class MUL(rd : Reg, rs : Reg)                      extends IR
  case class MOD(rd : Reg, rs : Reg, locals : Int)        extends IR

  // Comparison Binary Operators
  case class CMP(rd: Reg, rn: Reg)   extends IR
  case class CMPImm(rd: Reg, i: Int) extends IR

  // Logical Binary Operators
  case class AND(rd: Reg, rn: Reg, label: String) extends IR
  case class OR(rd: Reg, rn: Reg, label: String)  extends IR

  // Misc Statements
  case class LDR(rd : Reg, rs : Reg, offset : Int, flag : String) extends IR
  case class STR(rd : Reg, rs : Reg, offset : Int) extends IR
  case class FETCHINDEX(rd: Reg, rb: Reg, ri: Reg, elemSize: Int) extends IR
  case class StringInit(reg: Reg, stringNum: Int)  extends IR

}

object Registers {
  sealed trait Location
  case class Stack(offset : Int) extends Location
  sealed trait Reg extends Location

  case object R0 extends Reg {
    override def toString = "r0"
  }

  case object R1 extends Reg {
    override def toString = "r1"
  }

  case object R2 extends Reg {
    override def toString = "r2"
  }

  case object R3 extends Reg {
    override def toString = "r3"
  }

  case object R4 extends Reg {
    override def toString = "r4"
  }

  case object R5 extends Reg {
    override def toString = "r5"
  }

  case object R6 extends Reg {
    override def toString = "r6"
  }

  case object R7 extends Reg {
    override def toString = "r7"
  }

  case object R8 extends Reg {
    override def toString = "r8"
  }

  case object R9 extends Reg {
    override def toString = "r9"
  }

  case object R10 extends Reg {
    override def toString = "r10"
  }

  case object R12 extends Reg {
    override def toString = "r12"
  }

  case object R13 extends Reg {
    override def toString = "r13"
  }

  case object R14 extends Reg {
    override def toString = "r14"
  }

  case object FP extends Reg {
    override def toString = "fp"
  }

  case object LR extends Reg {
    override def toString = "lr"
  }

  case object PC extends Reg {
    override def toString = "pc"
  }

  case object SP extends Reg {
    override def toString = "sp"
  }

  val scratchReg1: Reg = R8
  val scratchReg2: Reg = R9

}
