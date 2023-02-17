package wacc

import wacc.IR._
import wacc.Registers._
object armPrinter {

  // TODO if we have a PUSH R8 followed by POP R8 remove that during printing - small optimisations

  def print(cg : codeGenerator) : String = {
     val sb = new StringBuilder()
     cg.generateProgIR().foreach(ir => sb.append(printIR(ir, cg) + "\n"))
     sb.toString()
  }

  def printIR(ir: IR, cg: codeGenerator): String = {
    ir match {

      case Data() => {
        val s = new StringBuilder(".data\n")
        for (i <- 0 until cg.stringNum) {
          s ++= s"@ length of .L.str$i\n"
          s ++= "   .word " + cg.strings(i).length.toString + "\n"
          s ++= s".L.str$i\n"
          s ++= "   .asciz \"" + cg.strings(i) + "\"\n"
        }
        s.toString()

        // TODO print strings
      }
      case Text() => ".text"
      case Global(globals) => ".global" + globals.foldLeft("")((x, y) => x + " " + y)

      // Label and Branch Statements
      case Label(label) => label + ":"
      case BL(label) => "bl " + label
      case BNE(label) => "bne " + label
      case BEQ(label) => "beq " + label
      case BUC(label) => "buc " + label

      // Move statements
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

      // Push and Pop Statements
      case PUSHMul(regs) => "push {" + regs.map(reg => reg.toString).mkString(",") + "}"
      case PUSH(reg) => "push {" + reg + "}"
      case POPMul(regs) => "pop {" + regs.map(reg => reg.toString).mkString(",") + "}"
      case POP(reg) => "pop {" + reg + "}"

      // Unary Operators TODO


      // Arithmetic Binary Operators
      case ADD(rd, rn, i) => "add " + rd + ", " + rn + ", #" + i
      case SUB(rd, rn, i) => "sub " + rd + ", " + rn + ", #" + i //TODO check order
      case DIV(rd, rs) => {
        val sb = new StringBuilder
        if (cg.numLocals() > 4) {
          sb.append(printIR(PUSHMul(List(R0, R1)), cg) + "\n")
        }
        sb.append(printIR(MOV(R0, rd), cg) + "\n")
        sb.append(printIR(MOV(R1, rs), cg) + "\n")
        sb.append(printIR(BL("__aeabi_idivmod"),   cg) + "\n")
        sb.append(printIR(PUSH(R0), cg) + "\n")

        if (cg.numLocals() > 4) {
          sb.append(printIR(POPMul(List(R0, R1)), cg))
        }

        sb.toString()
      }
      case MUL(rd, rs) => ""   // TODO ask Jamie about cmp r8, r9 asm #31
      case MOD(rd, rs) => {val sb = new StringBuilder
        if (cg.numLocals() > 4) {
          sb.append(printIR(PUSHMul(List(R0, R1)), cg) + "\n")
        }
        sb.append(printIR(MOV(R0, rd), cg) + "\n")
        sb.append(printIR(MOV(R1, rs), cg) + "\n")
        sb.append(printIR(BL("__aeabi_idivmod"), cg) + "\n")
        sb.append(printIR(PUSH(R1), cg) + "\n")

        if (cg.numLocals() > 4) {
          sb.append(printIR(POPMul(List(R0, R1)), cg))
        }

        sb.toString()
      }

      // Comparison Binary Operators
      case CMPImm(rd, i) => "cmp " + rd.toString + ", #" + i.toString
      case CMP(rd, rn)   => "cmp " + rd + ", " + rn

      // Logical Binary Operators
      case AND(rd, rn, label: String) => {
        val sb = new StringBuilder
        sb.append(printIR(CMPImm(rd,1), cg))
        sb.append(printIR(BNE(label), cg))
        sb.append(printIR(CMPImm(rn, 1), cg))
        sb.append(label)
        sb.append(printIR(MOVImm(rd, 1, "EQ"), cg))
        sb.append(printIR(MOVImm(rd, 0, "NE"), cg))
        sb.toString()
      }
      case OR(rd, rn, label: String) => {
        val sb = new StringBuilder
        sb.append(printIR(CMPImm(rd,1), cg))
        sb.append(printIR(BEQ(label), cg))
        sb.append(printIR(CMPImm(rn, 1), cg))
        sb.append(label)
        sb.append(printIR(MOVImm(rd, 1, "EQ"), cg))
        sb.append(printIR(MOVImm(rd, 0, "NE"), cg))
        sb.toString()
      }
      case _ => "Unreachable"
    }
  }

}
