package wacc

import wacc.IR._
import wacc.Registers._

object armPrinter {

  private val NewLineChar = "\n"


  def print(cg: codeGenerator): String = {
    val sb = new StringBuilder()
    cg.generateProgIR().foreach(ir => sb.append(printIR(ir) + NewLineChar))
    sb.toString()
  }

  def printInstr(Instr: String, regs: List[Reg]): String = {
    Instr + regs.map(reg => reg.toString).mkString(", ")
  }

  def printInstr(Instr: String, reg1: Reg, IntVal: Int): String = {
    Instr + reg1 + ", #" + IntVal
  }

  def printInstr(Instr: String, reg1: Reg, reg2: Reg, IntVal: Int): String = {
    Instr + reg1 + ", " + reg2 + ", #" + IntVal
  }

  def printIR(ir: IR): String = {
    ir match {
      case Data(strings: List[String], startIndex: Int) => {
        val s = new StringBuilder("\n.data\n")
        for (i <- strings.indices) {
          s ++= "   .word " + strings(i).length.toString + NewLineChar
          val count = i + startIndex
          s ++= s".L.str$count:\n"
          s ++= "   .asciz \"" + strings(i).replace("\"", "\\\"").replace("\n", "\\n") + "\"\n"
        }
        s ++= ".text"
        s.toString()
      }
      case Global(globals) => ".global" + globals.foldLeft("")((x, y) => x + " " + y)
      case LOCALCOLLECT => "   .ltorg\n"


      // Label and Branch Statements
      case Label(label) => "\n" + label + ":"
      case BRANCH(label, suffix) => {
        suffix match {
          case DEFAULT => "b " + label
          case NE => "bne " + label
          case EQ => "beq " + label
          case L => "bl " + label
          case LEQ => "bleq " + label
          case LNE => "blne " + label
          case LLT => "bllt " + label
          case LGE => "blge " + label
          case LVS => "blvs " + label
          case _ => "WTH WRONG INSTRUCTION"
        }
      }

      // Move statements
      case MOV(rd, rs, suffix) => {
        suffix match {
          case DEFAULT => printInstr("mov ", List(rd, rs))
          case GT => printInstr("movgt ", List(rd, rs))
          case LT => printInstr("movlt ", List(rd, rs))
          case GE => printInstr("movge ", List(rd, rs))
          case LE => printInstr("movle ", List(rd, rs))
          case EQ => printInstr("moveq ", List(rd, rs))
          case NE => printInstr("movne ", List(rd, rs))
          case _ => "WTH2 WRONG INSTRUCTION"
        }
      }

      case MOVImm(rd, i, suffix) => {
        suffix match {
          case DEFAULT => {
            if (i > 127 || i < -128) ("ldr " + rd + ", =" + i) else printInstr("mov ", rd, i)
          }
          case GT => printInstr("movgt ", rd, i)
          case LT => printInstr("movlt ", rd, i)
          case GE => printInstr("movge ", rd, i)
          case LE => printInstr("movle ", rd, i)
          case EQ => printInstr("moveq ", rd, i)
          case NE => printInstr("movne ", rd, i)
          case _ => printInstr("mov ", rd, i)
        }
      }

      // Push and Pop Statements
      case PUSHMul(regs) => "push {" + regs.sortWith((r1, r2) => r1.toString.compareTo(r2.toString) < 0).map(reg => reg.toString).mkString(", ") + "}"
      case PUSH(reg) => "push {" + reg + "}"
      case POPMul(regs) => "pop {" + regs.sortWith((r1, r2) => r1.toString.compareTo(r2.toString) < 0).map(reg => reg.toString).mkString(", ") + "}"
      case POP(reg) => "pop {" + reg + "}"

      // Unary Operators
      case NEG(rd, rs) => {
        val sb = new StringBuilder()
        sb.append(printInstr("rsbs ", rd, rs, 0) + NewLineChar)
        sb.append(printIR(BRANCH("_errOverflow", LVS)) + NewLineChar)
        sb.toString()
      }
      case NOT(rd, rs) => {
        val sb = new StringBuilder()
        sb.append(printIR(CMPImm(rs, 0)) + NewLineChar)
        sb.append(printIR(MOVImm(rs, 1, EQ)) + NewLineChar)
        sb.append(printIR(MOVImm(rs, 0, NE)) + NewLineChar)
        sb.append(printIR(MOV(rd, rs, DEFAULT)))
        sb.toString()
      }

      // Arithmetic Binary Operators
      case ADD(rd, rn, i, flag) => {
        flag match {
          case S => printInstr("adds ", rd, rn, i) + NewLineChar
          case DEFAULT => printInstr("add ", rd, rn, i)
        }
      }
      case ADDREG(rd, rn, rm) => {
        val sb = new StringBuilder
        sb.append(printInstr("adds ", List(rd, rn, rm)) + NewLineChar)
        sb.append(printIR(BRANCH("_errOverflow", LVS)))
        sb.toString()
      }
      case SUB(rd, rn, i) => {
        val sb = new StringBuilder
        sb.append(printInstr("subs ", rd, rn, i) + NewLineChar)
        sb.toString()
      }
      case SUBREG(rd, rn, rm) => {
        val sb = new StringBuilder
        sb.append(printInstr("subs ", List(rd, rn, rm)) + NewLineChar)
        sb.append(printIR(BRANCH("_errOverflow", LVS)) + NewLineChar)
        sb.toString()
      }
      case DIV(rd, rs, willClobber) => {
        val sb = new StringBuilder
        if (willClobber) {
          sb.append(printIR(PUSHMul(List(R0, R1))) + NewLineChar)
        }
        sb.append(printIR(MOV(R0, rd, DEFAULT)) + NewLineChar)
        sb.append(printIR(MOV(R1, rs, DEFAULT)) + NewLineChar)
        sb.append(printIR(CMPImm(R1, 0)) + NewLineChar)
        sb.append(printIR(BRANCH("_errDivZero", LEQ)) + NewLineChar)
        sb.append(printIR(BRANCH("__aeabi_idivmod", L)) + NewLineChar)
        sb.append(printIR(MOV(scratchReg1, R0, DEFAULT)) + NewLineChar)
        if (willClobber) {
          sb.append(printIR(POPMul(List(R0, R1))) + NewLineChar)

        }
        sb.append(printIR(PUSH(scratchReg1)))
        sb.toString()
      }

      case MUL(rd, rs) => {
        val sb = new StringBuilder()
        sb.append(printInstr("smull ", List(rd, rs, rd, rs)) + NewLineChar) // rd - low, rs - high
        sb.append(printIR(CMP(rs, rd)) + ", asr #31" + NewLineChar)
        sb.append(printIR(BRANCH("_errOverflow", LNE)) + NewLineChar)
        sb.toString()
      }
      case MOD(rd, rs, willClobber) => {
        val sb = new StringBuilder()
        if (willClobber) {
          sb.append(printIR(PUSHMul(List(R0, R1))) + NewLineChar)
        }
        sb.append(printIR(MOV(R0, rd, DEFAULT)) + NewLineChar)
        sb.append(printIR(MOV(R1, rs, DEFAULT)) + NewLineChar)
        sb.append(printIR(CMPImm(R1, 0)) + NewLineChar)
        sb.append(printIR(BRANCH("_errDivZero", LEQ)) + NewLineChar)
        sb.append(printIR(BRANCH("__aeabi_idivmod", L)) + NewLineChar)
        sb.append(printIR(MOV(scratchReg1, R1, DEFAULT)) + NewLineChar)
        if (willClobber) {
          sb.append(printIR(POPMul(List(R0, R1))) + NewLineChar)
        }
        sb.append(printIR(PUSH(scratchReg1)))
        sb.toString()
      }

      // Comparison Binary Operators
      case CMP(rd, rn) => printInstr("cmp ", List(rd, rn))
      case CMPImm(rd, i) => printInstr("cmp ", rd, i)

      // Logical Binary Operators
      case AND(rd, rn, label: String) => {
        val sb = new StringBuilder
        sb.append(printIR(CMPImm(rd, 1)) + NewLineChar)
        sb.append(printIR(BRANCH(label, NE)) + NewLineChar)
        sb.append(printIR(CMPImm(rn, 1)) + NewLineChar)
        sb.append(printIR(Label(label)) + NewLineChar)
        sb.append(printIR(MOVImm(rd, 1, EQ)) + NewLineChar)
        sb.append(printIR(MOVImm(rd, 0, NE)))
        sb.toString()
      }
      case OR(rd, rn, label: String) => {
        val sb = new StringBuilder
        sb.append(printIR(CMPImm(rd, 1)) + NewLineChar)
        sb.append(printIR(BRANCH(label, EQ)) + NewLineChar)
        sb.append(printIR(CMPImm(rn, 1)) + NewLineChar)
        sb.append(printIR(Label(label)) + NewLineChar)
        sb.append(printIR(MOVImm(rd, 1, EQ)) + NewLineChar)
        sb.append(printIR(MOVImm(rd, 0, NE)))
        sb.toString()
      }
      case TRUNCATE(rd, rs, i) => printInstr("and ", rs, rd, i)

      // Mem access Statements
      case LDR(rd, rs, offset, flag) => {
        flag match {
          case DEFAULT => "ldr " + rd + ", [" + rs + ", #" + offset + "]"
          case BYTECONSTOFFSET => "ldrsb " + rd + ", [" + rs + ", #" + offset + "]"
          case BYTEREGOFFSET => "ldrsb " + rd + ", [" + rd + ", " + rs + "]"
          case INDEX => "ldr " + rd + ", [" + rd + ", " + rs + ", lsl #" + offset + "]"
        }
      }

      case STR(rd, rs, offset, flag) => {
        flag match {
          case DEFAULT => "str " + rd + ", [" + rs + ", #" + offset + "]"
          case BYTECONSTOFFSET => "strb " + rd + ", [" + rs + ", #" + offset + "]"
          case _ => ""
        }

      }
      case STOREINDEX(rd: Reg, rb: Reg, ri: Reg, elemSize: Int)
      => "str " + rd + ", [" + rb + ", " + ri + ", lsl #" + elemSize + "]"

      case STOREINDEXB(rd: Reg, rb: Reg, ri: Reg)
      => "strb " + rd + ", [" + rb + ", " + ri + "]"

      case StringInit(reg, stringNum) => "ldr " + reg + ", " + "=.L.str" + stringNum
      case _ => "Unreachable"
    }
  }

}
