package wacc

import wacc.IR._

object armPrinter {

  def print(cg : codeGenerator) : String = {
     val sb = new StringBuilder()
     cg.generateProgIR().foreach(ir => sb.append(printIR(ir) + "\n"))
     sb.toString()
  }

  def printIR(ir: IR): String = {
    ir match {
      case Data() => ".data"
      case Text() => ".text"
      case Global(globals) => ".global" + globals.foldLeft("")((x, y) => x + " " + y)
      case POPMul(regs) => "pop {" + regs.map(reg => reg.toString).mkString(",") + "}"
      case POP(reg) => "pop {" + reg + "}"
      case PUSHMul(regs) => "push {" + regs.map(reg => reg.toString).mkString(",") + "}"
      case PUSH(reg) => "push {" + reg + "}"
      case MOV(rd, rs) => "mov " + rd + ", " + rs
      case MOVImm(rd, i, flag) => {
        flag match {
          case "Default" => "mov " + rd + ", #" + i
          case "GT" => "movgt " + rd + ", #" + i
          case "LT" => "movlt " + rd + ", #" + i
          case "GT" => "movge " + rd + ", #" + i
          case "LE" => "movle " + rd + ", #" + i
          case "EQ" => "moveq " + rd + ", #" + i
          case "NE" => "movne " + rd + ", #" + i
        }
      }
      case Label(label) => label + ":"
      case BL(label) => "bl " + label

      //binOps
      case ADD(rd, rn, i) => "add " + rd + ", " + rn + ", #" + i
      case SUB(rd, rn, i) => "sub " + rd + ", " + rn + ", #" + i
      case MUL(rd, rs) => ""   // TODO ask Jamie about cmp r8, r9 asm #31
      case DIV(rd, rs) => {
        val sb = new StringBuilder
        sb += "mov" + rd + ", " + "R8" + "\n"
        sb += "mov" + rs + ", " + "R9" + "\n"
        sb += "bl __aeabi_idivmod"
        sb += "push R0"
      }
      case MOD(rd, rs) => {
        val sb = new StringBuilder
        sb += "mov" + rd + ", " + "R8" + "\n"
        sb += "mov" + rs + ", " + "R9" + "\n"
        sb += "bl __aeabi_idivmod"
        sb += "push R1"
      }
      case CMPImm(rd, i) => "cmp" + rd.toString + ", #" + i.toString
      case CMP(rd, rn) => "cmp " + rd + ", " + rn
//        List(POP(R8), POP(R9), CMPImm(R8, 1), BNE(label), CMPImm(R9, 1), Label(label), MOVImm(R8, 1, "EQ"), MOVImm
      case AND(rd, rn, label: Label) => {
        val sb = new StringBuilder
        sb += printIR(CMPImm(rd,1))
        sb += "bne" + label
        sb += printIR(CMPImm(rn, 1))
        sb += label + ":"
        sb += printIR(MOVImm(rd, 1, "EQ"))
        sb += printIR(MOVImm(rd, 0, "NE"))
      }
      case OR(rd, rn, label: Label) => {
        val sb = new StringBuilder
        sb += printIR(CMPImm(rd,1))
        sb += "beq" + label
        sb += printIR(CMPImm(rn, 1))
        sb += label + ":"
        sb += printIR(MOVImm(rd, 1, "EQ"))
        sb += printIR(MOVImm(rd, 0, "NE"))
      }
      case x => "Unreachable"
    }
  }

}
