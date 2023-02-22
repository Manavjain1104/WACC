package wacc

import wacc.IR._
import wacc.Registers._
object armPrinter {

  private val NewLineChar = "\n"

  def print(cg : codeGenerator) : String = {
     val sb = new StringBuilder()
     cg.generateProgIR().foreach(ir => sb.append(printIR(ir) + NewLineChar))
     sb.toString()
  }

  def printInstr(Instr : String, regs : List[Reg]) : String = {
    Instr + regs.map(reg => reg.toString).mkString(", ")
  }
  def printInstr(Instr : String, reg1 : Reg, IntVal : Int) : String = {
       Instr + reg1 + ", #" + IntVal
     }
  def printInstr(Instr : String, reg1 : Reg, reg2 : Reg,  IntVal : Int) : String = {
    Instr + reg1 + ", " + reg2 +", #" + IntVal
  }

  def printIR(ir: IR): String = {
    ir match {
      case Data(strings: List[String], startIndex : Int) => {
        val s = new StringBuilder("\n.data\n")
        for (i <- strings.indices) {
          s ++= "   .word " + strings(i).length.toString + NewLineChar
          val count = i + startIndex
          s ++= s".L.str$count:\n"
          s ++= "   .asciz \"" + strings(i).replace("\"", "\\\"") + "\"\n"
        }
        s ++= ".text"
        s.toString()
      }
      case Global(globals) => ".global" + globals.foldLeft("")((x, y) => x + " " + y)
      case LTORG => "   .ltorg\n"


      // Label and Branch Statements
      case Label(label) => "\n" + label + ":"
      case BRANCH(label, suffix) => {
        suffix match {
          case "Default" => "b " + label
          case "NE"      => "bne " + label
          case "EQ"      => "beq " + label
          case "L"       => "bl "  + label
          case "LEQ"       => "bleq "  + label
          case "LNE"     => "blne " + label
          case "LLT"     => "bllt " + label
          case "LGE"     => "blge " + label
          case "LVS"     => "blvs " + label
          case _         => "WTH WRONG INSTRUCTION"
        }
      }

      // Move statements
      case MOV(rd, rs, flag) => {
        flag match {
          case "Default" => printInstr("mov ", List(rd, rs))
          case "GT"      => printInstr("movgt ", List(rd, rs))
          case "LT"      => printInstr("movlt ", List(rd, rs))
          case "GE"      => printInstr("movge ", List(rd, rs))
          case "LE"      => printInstr("movle ", List(rd, rs))
          case "EQ"      => printInstr("moveq ", List(rd, rs))
          case "NE"      => printInstr("movne ", List(rd, rs))
          case _         => "WTH2 WRONG INSTRUCTION"
        }
      }

      case MOVImm(rd, i, flag) => {
        flag match {
          case "Default" => printInstr("mov ", rd, i)
          case "GT"      => printInstr("movgt ", rd, i)
          case "LT"      => printInstr("movlt ", rd, i)
          case "GE"      => printInstr("movge ", rd, i)
          case "LE"      => printInstr("movle ", rd, i)
          case "EQ"      => printInstr("moveq ", rd, i)
          case "NE"      => printInstr("movne ", rd, i)
          case _         => printInstr("mov ", rd, i)
        }
      }

      // Push and Pop Statements
      case PUSHMul(regs) => "push {" + regs.map(reg => reg.toString).mkString(", ") + "}"
      case PUSH(reg)     => "push {" + reg + "}"
      case POPMul(regs)  => "pop {" + regs.map(reg => reg.toString).mkString(", ") + "}"
      case POP(reg)      => "pop {" + reg + "}"

      // Unary Operators
      case NEG(rd, rs) => printInstr("rsbs ", rd, rs, 0)
      case NOT(rd, rs) => {
        val sb = new StringBuilder()
        sb.append(printIR(CMPImm(rs, 0)) + NewLineChar)
        sb.append(printIR(MOVImm(rs, 1, "EQ")) + NewLineChar)
        sb.append(printIR(MOVImm(rs, 0, "NE")) + NewLineChar)
        sb.append(printIR(MOV(rd, rs, "Default")))
        sb.toString()
      }

      // Arithmetic Binary Operators
      case ADD(rd, rn, i, flag)     => {
        flag match {
          case "s" => printInstr("adds ", rd, rn, i)
          case _ => printInstr("add ", rd, rn, i)
        }

      }
      case ADDREG(rd, rn, rm) => printInstr("adds ", List(rd, rn, rm))
      case SUB(rd, rn, i)     => printInstr("subs ", rd, rn, i)
      case SUBREG(rd, rn, rm) => printInstr("subs ", List(rd, rn, rm))
      case DIV(rd, rs, locals) => {
        val sb = new StringBuilder
        if (locals > 4) {
          sb.append(printIR(PUSHMul(List(R0, R1))) + NewLineChar)
        }
        sb.append(printIR(MOV(R0, rd, "Default")) + NewLineChar)
        sb.append(printIR(MOV(R1, rs, "Default")) + NewLineChar)
//        sb.append(printIR(CMPImm(R1, 0)))
//        sb.append(printIR(BRANCH("_errDivZero", "EQ")))
        sb.append(printIR(BRANCH("__aeabi_idivmod", "L")) + NewLineChar)
        sb.append(printIR(PUSH(R0)) + NewLineChar)

        if (locals > 4) {
          sb.append(printIR(POPMul(List(R0, R1))))
        }

        sb.toString()
      }
      case MUL(rd, rs) => {
        val sb = new StringBuilder()
        sb.append(printInstr("smull ", List(rd, rs, rd, rs)) ) // rd - low, rs - high
//        sb.append(printIR(CMP(rs, rd)) + "asr #31\n")
        sb.toString()
      }
      case MOD(rd, rs, locals) => {
        val sb = new StringBuilder()
        if (locals > 4) {
          sb.append(printIR(PUSHMul(List(R0, R1))) + NewLineChar)
        }
        sb.append(printIR(MOV(R0, rd, "Default")) + NewLineChar)
        sb.append(printIR(MOV(R1, rs, "Default")) + NewLineChar)
//        sb.append(printIR(CMPImm(R1, 0)))
//        sb.append(printIR(BRANCH("_errDivZero", "EQ")))
        sb.append(printIR(BRANCH("__aeabi_idivmod", "L")) + NewLineChar)
        sb.append(printIR(PUSH(R1)) + NewLineChar)

        if (locals > 4) {
          sb.append(printIR(POPMul(List(R0, R1))))
        }

        sb.toString()
      }

      // Comparison Binary Operators
      case CMP(rd, rn)   => printInstr("cmp ", List(rd , rn))
      case CMPImm(rd, i) => printInstr("cmp ", rd, i)

      // Logical Binary Operators
      case AND(rd, rn, label: String) => {
        val sb = new StringBuilder
        sb.append(printIR(CMPImm(rd,1)) + NewLineChar)
        sb.append(printIR(BRANCH(label, "NE")) + NewLineChar)
        sb.append(printIR(CMPImm(rn, 1)) + NewLineChar)
        sb.append(printIR(Label(label)) + NewLineChar)
        sb.append(printIR(MOVImm(rd, 1, "EQ")) + NewLineChar)
        sb.append(printIR(MOVImm(rd, 0, "NE")))
        sb.toString()
      }
      case OR(rd, rn, label: String) => {
        val sb = new StringBuilder
        sb.append(printIR(CMPImm(rd,1)) + NewLineChar)
        sb.append(printIR(BRANCH(label, "EQ")) + NewLineChar)
        sb.append(printIR(CMPImm(rn, 1)) + NewLineChar)
        sb.append(printIR(Label(label)) + NewLineChar)
        sb.append(printIR(MOVImm(rd, 1, "EQ")) + NewLineChar)
        sb.append(printIR(MOVImm(rd, 0, "NE")))
        sb.toString()
      }
      case TRUNCATE(rd, rs, i) => printInstr("and ", rs, rd, i)

      // Misc Statements
      case LDR(rd, rs, offset, flag) => {
        flag match {
          case "Default" => "ldr " + rd +", [" + rs + ", #" + offset + "]"
          case "sb"      => "ldrsb " + rd +", [" + rs + ", #" + offset + "]"
          case "index"     => "ldr " + rd + ", ["+ rd + ", " + rs + ", lsl #" + offset + "]"
          case "sbReg"   => "ldrsb " + rd +", [" + rd + ", " + rs + "]"
        }
      }


      case STR(rd, rs, offset, flag) => {
        flag match {
          case "b" => "strb " + rd +", [" + rs + ", #" + offset + "]"
          case _ => "str " + rd +", [" + rs + ", #" + offset + "]"
        }

      }
      case STOREINDEX(rd : Reg, rb : Reg, ri : Reg, elemSize : Int)
        => "str " + rd +", [" + rb + ", " + ri + ", lsl #" + elemSize + "]"
      case STOREINDEXB(rd: Reg, rb: Reg, ri: Reg)
      => "strb " + rd + ", ["+ rb + ", " + ri + "]"
      case StringInit(reg, stringNum) => "ldr " + reg + ", " + "=.L.str" + stringNum
      case _ => "Unreachable"
    }
  }

}
