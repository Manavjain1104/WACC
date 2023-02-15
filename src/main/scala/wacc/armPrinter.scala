package wacc

import wacc.IR._

object armPrinter {

  def print(irs : List[IR]) : String = {
     val sb = new StringBuilder()
     for (ir <- irs ){
       sb.append(printIR(ir) + "\n")
     }
     sb.toString()
  }

  def printIR(ir: IR): String = {
    ir match {
      case Data() => ".data"
      case Text() => ".text"
      case Global(globals) => ".global" + globals.foldLeft("")((x, y) => x + " " + y)
      case POP(regs) => "pop {" + regs.map(reg => reg.toString).mkString(",") + "}"
      case PUSH(regs) => "push {" + regs.map(reg => reg.toString).mkString(",") + "}"
      case MOV(rd, rs) => "mov " + rd.toString + ", " + rs.toString
      case MOVImm(rd, i) => "mov " + rd.toString + ", #" + i.toString
      case Label(label) => label + ":"
      case BL(label) => "bl " + label
      case _ => "Unreachable"
    }
  }

}
