package wacc

import wacc.IR._
import wacc.Registers._
object armPrinter {

  def print(cg : codeGenerator) : String = {
     val sb = new StringBuilder()
     cg.generateProgIR().foreach(ir => sb.append(printIR(ir) + "\n"))
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
        val s = new StringBuilder(".data\n")
        for (i <- strings.indices) {
          s ++= "   .word " + strings(i).length.toString + "\n"
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
      case Label(label) => label + ":"
      case BNE(label) => "bne " + label
      case BEQ(label) => "beq " + label
      case BUC(label) => "b " + label
      case BL(label) => "bl " + label
      case BLNE(label) => "blne " + label

      // Move statements
      case MOV(rd, rs) => "mov " + rd + ", " + rs
      case MOVImm(rd, i, flag) => {
        flag match {
          case "Default" => printInstr("mov ", rd, i)
          case "GT"      => printInstr("movgt ", rd, i)
          case "LT"      => printInstr("movlt ", rd, i)
          case "GE"      => printInstr("movge ", rd, i)
          case "LE"      => printInstr("movle ", rd, i)
          case "EQ"      => printInstr("moveq ", rd, i)
          case "NE"      => printInstr("movne ", rd, i)
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
        sb.append(printIR(CMPImm(rs, 0)) + "\n")
        sb.append(printIR(MOVImm(rs, 1, "EQ")) + "\n")
        sb.append(printIR(MOVImm(rs, 0, "NE")) + "\n")
        sb.append(printIR(MOV(rd, rs)))
        sb.toString()
      }

      // Arithmetic Binary Operators
      case ADD(rd, rn, i)     => printInstr("adds ", rd, rn, i)
      case ADDREG(rd, rn, rm) => printInstr("adds ", List(rd, rn, rm))
      case SUB(rd, rn, i)     => printInstr("subs ", rd, rn, i) //TODO check order
      case SUBREG(rd, rn, rm) => printInstr("subs ", List(rd, rn, rm))
      case DIV(rd, rs, locals) => {
        val sb = new StringBuilder
        if (locals > 4) {
          sb.append(printIR(PUSHMul(List(R0, R1))) + "\n")
        }
        sb.append(printIR(MOV(R0, rd)) + "\n")
        sb.append(printIR(MOV(R1, rs)) + "\n")
        sb.append(printIR(BL("__aeabi_idivmod")) + "\n")
        sb.append(printIR(PUSH(R0)) + "\n")

        if (locals > 4) {
          sb.append(printIR(POPMul(List(R0, R1))))
        }

        sb.toString()
      }
      case MUL(rd, rs) => {
        val sb = new StringBuilder()
        sb.append(printInstr("smull ", List(rd, rs, rd, rs)) ) // rd - low, rs - high
//        sb.append(printIR(CMP(rs, rd),cg) + "asr #31\n")
//        sb.append(printIR(BLNE("_errOverflow"),cg)) // TODO overflow
        sb.toString()
      }
      case MOD(rd, rs, locals) => {
        val sb = new StringBuilder()
        if (locals > 4) {
          sb.append(printIR(PUSHMul(List(R0, R1))) + "\n")
        }
        sb.append(printIR(MOV(R0, rd)) + "\n")
        sb.append(printIR(MOV(R1, rs)) + "\n")
        sb.append(printIR(BL("__aeabi_idivmod")) + "\n")
        sb.append(printIR(PUSH(R1)) + "\n")

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
        sb.append(printIR(CMPImm(rd,1)) + "\n")
        sb.append(printIR(BNE(label)) + "\n")
        sb.append(printIR(CMPImm(rn, 1)) + "\n")
        sb.append(printIR(Label(label)) + "\n")
        sb.append(printIR(MOVImm(rd, 1, "EQ")) + "\n")
        sb.append(printIR(MOVImm(rd, 0, "NE")))
        sb.toString()
      }
      case OR(rd, rn, label: String) => {
        val sb = new StringBuilder
        sb.append(printIR(CMPImm(rd,1)) + "\n")
        sb.append(printIR(BEQ(label)) + "\n")
        sb.append(printIR(CMPImm(rn, 1)) + "\n")
        sb.append(printIR(Label(label)) + "\n")
        sb.append(printIR(MOVImm(rd, 1, "EQ")) + "\n")
        sb.append(printIR(MOVImm(rd, 0, "NE")))
        sb.toString()
      }

      // Misc Statements
      case LDR(rd, rs, offset, flag) => {
        flag match {
          case "Default" => "ldr " + rd +", [" + rs + ", #" + offset + "]"
          case "sb"      => "ldrsb " + rd +", [" + rs + ", #" + offset + "]"
        }
      }
      case STR(rd, rs, offset)        => "str " + rd +", [" + rs + ", #" + offset + "]"
      case StringInit(reg, stringNum) => "ldr " + reg + ", " + "=.L.str" + stringNum
      case _ => "Unreachable"
    }
  }

}
