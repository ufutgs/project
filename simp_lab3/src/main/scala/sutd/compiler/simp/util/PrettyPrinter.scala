package sutd.compiler.simp.util
import sutd.compiler.simp.ir.PseudoAssembly.*
import Instr.*
import Opr.* 
import scala.annotation.tailrec

object PrettyPrinter {
    
  def prettyPrint(pa:List[LabeledInstr]) : Unit = {
    @tailrec
    def recurPrint(list:List[String]) : Unit = list match{
        case Nil => 
        case head :: next => {
            println(head)
            recurPrint(next)
        }
    }
    val list = translate(pa)
    println("-------- Pretty-Printed PA --------")
    recurPrint(list)
  }

  def translate(pa:List[LabeledInstr]) : List[String] = pa.foldRight(List[String]())( (li : LabeledInstr,program:List[String]) =>li match{
    case (lnum,instr) => {
        val text = lnum.toString() +": "
        translate_Instr(text,instr) :: program
    }
  })

  def translate_Instr(text:String, li:Instr) : String = li match {
    case IMove(dest, src) =>text + translate_Opr(dest) + " <- " + translate_Opr(src)
    case IRet => text + "ret"
    case IGoto(dest) => text + "goto " + dest.toString()
    case IIfNot(cond, dest) => text + "ifn "+ translate_Opr(cond) + " goto "+ dest.toString()
    case IPlus(dest, src1, src2) => text + translate_Opr(dest) + " <- " + translate_Opr(src1) + " + " + translate_Opr(src2)
    case IMinus(dest, src1, src2) => text + translate_Opr(dest) + " <- " + translate_Opr(src1) + " - " + translate_Opr(src2) 
    case IMult(dest, src1, src2) => text + translate_Opr(dest) + " <- " + translate_Opr(src1) + " * " + translate_Opr(src2) 
    case IDEqual(dest, src1, src2) => text + translate_Opr(dest) + " <- " + translate_Opr(src1) + " == " + translate_Opr(src2) 
    case ILThan(dest, src1, src2) => text + translate_Opr(dest) + " <- " + translate_Opr(src1) + " < " + translate_Opr(src2) 
  }

  def translate_Opr(op: Opr) : String = op match {
    case Regstr(name) => name
    case Temp(AVar(name)) =>name 
    case IntLit(v) => v.toString() 
  }
}
