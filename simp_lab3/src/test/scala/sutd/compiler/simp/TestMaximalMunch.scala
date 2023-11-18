package sutd.compiler.simp

import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.simp.syntax.AST.*
import sutd.compiler.simp.ir.PseudoAssembly.*
import sutd.compiler.simp.ir.Util.{given, *}
import sutd.compiler.simp.ir.MaximalMunch.*
import sutd.compiler.simp.monad.StateT.{given, *}

class TestMaximalMunch extends funsuite.AnyFunSuite {
    import Stmt.*, Var.*, Exp.*, Const.*
    import Instr.*, Opr.*
    // y = 1
    // x = y + 1
    test("test maximal munch: y = 1; x = y + 1") {
        val stmt1 = Assign(Var("y"), ConstExp(IntConst(1)))
        val stmt2 = Assign(Var("x"), Plus(VarExp(Var("y")), ConstExp(IntConst(1))))
        val expected = List(
            (1, IMove(Temp(AVar("y")),IntLit(1))), 
            (2, IMove(Temp(AVar("var_1")),Temp(AVar("y")))), 
            (3, IMove(Temp(AVar("var_2")),IntLit(1))), 
            (4, IPlus(Temp(AVar("x")),Temp(AVar("var_1")),Temp(AVar("var_2")))))
        cogen(List(stmt1, stmt2)).run(StateInfo(1,"var",1)) match {
            case Identity((st_, instrs)) => {
                assert(expected == instrs)
            }
        }
    }    

    // z = (x + 3) * (y - 5)
    // return z
    test("test maximal munch: z = (x + 3) * (y - 5); return z") {
        val stmt3 = Assign(Var("z"), Mult(Plus(VarExp(Var("x")), ConstExp(IntConst(3))), Minus(VarExp(Var("y")), ConstExp(IntConst(5)))))
        val stmt4 = Ret(Var("z"))
        val expected = List(
            (1, IMove(Temp(AVar("var_3")),Temp(AVar("x")))), 
            (2, IMove(Temp(AVar("var_4")),IntLit(3))), 
            (3, IPlus(Temp(AVar("var_1")),Temp(AVar("var_3")),Temp(AVar("var_4")))), 
            (4, IMove(Temp(AVar("var_5")),Temp(AVar("y")))), 
            (5, IMove(Temp(AVar("var_6")),IntLit(5))), 
            (6, IMinus(Temp(AVar("var_2")),Temp(AVar("var_5")),Temp(AVar("var_6")))), 
            (7, IMult(Temp(AVar("z")),Temp(AVar("var_1")),Temp(AVar("var_2")))), 
            (8, IMove(Regstr("_r_ret"),Temp(AVar("z")))), 
            (9, IRet))
        cogen(List(stmt3, stmt4)).run(StateInfo(1,"var",1)) match {
            case Identity((st_, instrs)) => {
                // println(instrs)
                assert(expected == instrs)
            }
        }
    }

    test("test maximal munch:" + """
    x = 0
    y = 10
    i = 0
    while (i < y) {
        x = x + i
        i = i + 1
    }
    return x
    """) {
        /* x = 0
           y = 10
           i = 0
           while (i < y) {
              x = x + i
              i = i + 1
           }
           return x
        */
        val stmt1 = Assign(Var("x"), ConstExp(IntConst(0)))
        val stmt2 = Assign(Var("y"), ConstExp(IntConst(10)))
        val stmt3 = Assign(Var("i"), ConstExp(IntConst(0)))
        val stmt4 = While(LThan(VarExp(Var("i")), VarExp(Var("y"))), List(
            Assign(Var("x"), Plus(VarExp(Var("x")), VarExp(Var("i")))),
            Assign(Var("i"), Plus(VarExp(Var("i")), ConstExp(IntConst(1))))
        ))
        val stmt5 = Ret(Var("x"))

        val expected = List(
            (1,IMove(Temp(AVar("x")),IntLit(0))), 
            (2,IMove(Temp(AVar("y")),IntLit(10))), 
            (3,IMove(Temp(AVar("i")),IntLit(0))), 
            (4,IMove(Temp(AVar("var_2")),Temp(AVar("i")))), 
            (5,IMove(Temp(AVar("var_3")),Temp(AVar("y")))), 
            (6,ILThan(Temp(AVar("var_1")),Temp(AVar("var_2")),Temp(AVar("var_3")))), 
            (7,IIfNot(Temp(AVar("var_1")),15)), 
            (8,IMove(Temp(AVar("var_4")),Temp(AVar("x")))), 
            (9,IMove(Temp(AVar("var_5")),Temp(AVar("i")))), 
            (10,IPlus(Temp(AVar("x")),Temp(AVar("var_4")),Temp(AVar("var_5")))), 
            (11,IMove(Temp(AVar("var_6")),Temp(AVar("i")))), 
            (12,IMove(Temp(AVar("var_7")),IntLit(1))), 
            (13,IPlus(Temp(AVar("i")),Temp(AVar("var_6")),Temp(AVar("var_7")))), 
            (14,IGoto(4)), 
            (15,IMove(Regstr("_r_ret"),Temp(AVar("x")))), 
            (16,IRet))

        
        cogen(List(stmt1, stmt2, stmt3, stmt4, stmt5)).run(StateInfo(1,"var",1)) match {
            case Identity((st_, instrs)) => {
                // println(instrs)
                assert(expected == instrs)
            }
        }
    }
}