package sutd.compiler.simp


import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.simp.ir.PseudoAssembly.*
import sutd.compiler.simp.ir.SSA.*


class TestSSA extends funsuite.AnyFunSuite {
    import Instr.* 
    import Opr.* 
    test("testing buildSSA 1") {
        val pa = List(
            (1,IMove(Temp(AVar("x")),IntLit(0))), 
            (2,IMove(Temp(AVar("y")),IntLit(10))), 
            (3,IMove(Temp(AVar("i")),IntLit(0))), 
            (4,ILThan(Temp(AVar("var_1")),Temp(AVar("i")),Temp(AVar("y")))), 
            (5,IIfNot(Temp(AVar("var_1")),11)), 
            (6,IPlus(Temp(AVar("var_2")),Temp(AVar("x")),Temp(AVar("i")))), 
            (7,IMove(Temp(AVar("x")),Temp(AVar("var_2")))), 
            (8,IPlus(Temp(AVar("var_3")),Temp(AVar("i")),IntLit(1))), 
            (9,IMove(Temp(AVar("i")),Temp(AVar("var_3")))), 
            (10,IGoto(4)), 
            (11,IMove(Regstr("_r_ret"),Temp(AVar("x")))), 
            (12,IRet))

        val expected = List(
            (1,List(),IMove(Temp(AVar("x_1")),IntLit(0))), 
            (2,List(),IMove(Temp(AVar("y_1")),IntLit(10))), 
            (3,List(),IMove(Temp(AVar("i_1")),IntLit(0))), 
            (4,List(
                PhiAssignment(Temp(AVar("i_2")),List((3,AVar("i_1")), (10,AVar("i_3"))),Temp(AVar("i"))), 
                PhiAssignment(Temp(AVar("x_2")),List((3,AVar("x_1")), (10,AVar("x_3"))),Temp(AVar("x")))
                ),ILThan(Temp(AVar("var_1_1")),Temp(AVar("i_2")),Temp(AVar("y_1")))), 
            (5,List(),IIfNot(Temp(AVar("var_1_1")),11)), 
            (6,List(),IPlus(Temp(AVar("var_2_1")),Temp(AVar("x_2")),Temp(AVar("i_2")))), 
            (7,List(),IMove(Temp(AVar("x_3")),Temp(AVar("var_2_1")))), 
            (8,List(),IPlus(Temp(AVar("var_3_1")),Temp(AVar("i_2")),IntLit(1))), 
            (9,List(),IMove(Temp(AVar("i_3")),Temp(AVar("var_3_1")))), 
            (10,List(),IGoto(4)), 
            (11,List(),IMove(Regstr("_r_ret"),Temp(AVar("x_2")))), 
            (12,List(),IRet))
        buildSSA(pa) match {
            case Left(err) => {
                println(err)
                assert(false)
            } 
            case Right(ssa) => assert (ssa == expected)
        }
    }



    test("testing buildSSA 2") {
        /*
        1: x <- input
        2: f <- 0
        3: s <- 1
        4: c <- 0
        5: t <- 0
        6: b <- c < x
        7: ifn b goto 13
        8: t <- f
        9: f <- s
        10: s <- t + f
        11: c <- c + 1
        12: goto 6
        13: r_ret <- s
        14: ret
        */
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val f = Temp(AVar("f"))
        val s = Temp(AVar("s"))
        val c = Temp(AVar("c"))
        val b = Temp(AVar("b"))
        val t = Temp(AVar("t"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IMove(f, IntLit(0))),
            (3, IMove(s, IntLit(1))),
            (4, IMove(c, IntLit(0))),
            (5, IMove(t, IntLit(0))),
            (6, ILThan(b, c, x)),
            (7, IIfNot(b, 13)),
            (8, IMove(t, f)),
            (9, IMove(f, s)),
            (10, IPlus(s, t, f)),
            (11, IPlus(c, c, IntLit(1))),
            (12, IGoto(6)),
            (13, IMove(r_ret, s)),
            (14, IRet)
        )
        val expected = List(
            (1,List(),IMove(Temp(AVar("x_1")),Temp(AVar("input")))), 
            (2,List(),IMove(Temp(AVar("f_1")),IntLit(0))), 
            (3,List(),IMove(Temp(AVar("s_1")),IntLit(1))), 
            (4,List(),IMove(Temp(AVar("c_1")),IntLit(0))), 
            (5,List(),IMove(Temp(AVar("t_1")),IntLit(0))), 
            (6,List(
                PhiAssignment(Temp(AVar("c_2")),List((5,AVar("c_1")), (12,AVar("c_3"))),Temp(AVar("c"))), 
                PhiAssignment(Temp(AVar("f_2")),List((5,AVar("f_1")), (12,AVar("f_3"))),Temp(AVar("f"))), 
                PhiAssignment(Temp(AVar("s_2")),List((5,AVar("s_1")), (12,AVar("s_3"))),Temp(AVar("s"))),
                PhiAssignment(Temp(AVar("t_2")),List((5,AVar("t_1")), (12,AVar("t_3"))),Temp(AVar("t"))) 
                ),ILThan(Temp(AVar("b_1")),Temp(AVar("c_2")),Temp(AVar("x_1")))), 
            (7,List(),IIfNot(Temp(AVar("b_1")),13)), 
            (8,List(),IMove(Temp(AVar("t_3")),Temp(AVar("f_2")))), 
            (9,List(),IMove(Temp(AVar("f_3")),Temp(AVar("s_2")))), 
            (10,List(),IPlus(Temp(AVar("s_3")),Temp(AVar("t_3")),Temp(AVar("f_3")))), 
            (11,List(),IPlus(Temp(AVar("c_3")),Temp(AVar("c_2")),IntLit(1))), 
            (12,List(),IGoto(6)), 
            (13,List(),IMove(Regstr("_r_ret"),Temp(AVar("s_2")))), 
            (14,List(),IRet)) 
        buildSSA(pa) match {
            case Left(err) => {
                println(err)
                assert(false)
            } 
            case Right(ssa) => assert (ssa == expected)
        }
    }


    test("testing buildSSA 3") {
        /*
        1: x <- input
        2: r <- 0
        3: i <- 0
        4: b <- i < x
        5: ifn b goto 20
        6: f <- 0
        7: s <- 1
        8: j <- 0
        9: t <- 0
        10: b <- j < i
        11: ifn b goto 17
        12: t <- f
        13: f <- s
        14: s <- t + f
        15: j <- j + 1
        16: goto 10
        17: r <- r + s
        18: i <- i + 1
        19: goto 4
        20: _ret_r <- r
        21: ret
        */
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val r = Temp(AVar("r"))
        val f = Temp(AVar("f"))
        val s = Temp(AVar("s"))
        val i = Temp(AVar("i"))
        val j = Temp(AVar("j"))
        val b = Temp(AVar("b"))
        val t = Temp(AVar("t"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IMove(r, IntLit(0))),
            (3, IMove(i, IntLit(0))),
            (4, ILThan(b, i, x)),
            (5, IIfNot(b, 20)),
            (6, IMove(f, IntLit(0))),
            (7, IMove(s, IntLit(1))),
            (8, IMove(j, IntLit(0))),
            (9, IMove(t, IntLit(0))),
            (10, ILThan(b, j, i)),
            (11, IIfNot(b, 17)),
            (12, IMove(t, f)),
            (13, IMove(f, s)),
            (14, IPlus(s, t, f)),
            (15, IPlus(j, j, IntLit(1))),
            (16, IGoto(10)),
            (17, IPlus(r, r, s)),
            (18, IPlus(i, i, IntLit(1))),
            (19, IGoto(4)),
            (20, IMove(r_ret, r)),
            (21, IRet)
        )
        val expected = List(
            (1,List(),IMove(Temp(AVar("x_1")),Temp(AVar("input")))), 
            (2,List(),IMove(Temp(AVar("r_1")),IntLit(0))), 
            (3,List(),IMove(Temp(AVar("i_1")),IntLit(0))), 
            (4,List(
                PhiAssignment(Temp(AVar("i_2")),List((3,AVar("i_1")), (19,AVar("i_3"))),Temp(AVar("i"))),
                PhiAssignment(Temp(AVar("r_2")),List((3,AVar("r_1")), (19,AVar("r_3"))),Temp(AVar("r"))) 
                ),ILThan(Temp(AVar("b_1")),Temp(AVar("i_2")),Temp(AVar("x_1")))), 
            (5,List(),IIfNot(Temp(AVar("b_1")),20)), 
            (6,List(),IMove(Temp(AVar("f_1")),IntLit(0))), 
            (7,List(),IMove(Temp(AVar("s_1")),IntLit(1))), 
            (8,List(),IMove(Temp(AVar("j_1")),IntLit(0))), 
            (9,List(),IMove(Temp(AVar("t_1")),IntLit(0))), 
            (10,List(
                PhiAssignment(Temp(AVar("b_2")),List((9,AVar("b_1")), (16,AVar("b_3"))),Temp(AVar("b"))), 
                PhiAssignment(Temp(AVar("f_2")),List((9,AVar("f_1")), (16,AVar("f_3"))),Temp(AVar("f"))), 
                PhiAssignment(Temp(AVar("j_2")),List((9,AVar("j_1")), (16,AVar("j_3"))),Temp(AVar("j"))), 
                PhiAssignment(Temp(AVar("s_2")),List((9,AVar("s_1")), (16,AVar("s_3"))),Temp(AVar("s"))),
                PhiAssignment(Temp(AVar("t_2")),List((9,AVar("t_1")), (16,AVar("t_3"))),Temp(AVar("t"))) 
                ),ILThan(Temp(AVar("b_3")),Temp(AVar("j_2")),Temp(AVar("i_2")))), 
            (11,List(),IIfNot(Temp(AVar("b_3")),17)), 
            (12,List(),IMove(Temp(AVar("t_3")),Temp(AVar("f_2")))), 
            (13,List(),IMove(Temp(AVar("f_3")),Temp(AVar("s_2")))), 
            (14,List(),IPlus(Temp(AVar("s_3")),Temp(AVar("t_3")),Temp(AVar("f_3")))), 
            (15,List(),IPlus(Temp(AVar("j_3")),Temp(AVar("j_2")),IntLit(1))), 
            (16,List(),IGoto(10)), 
            (17,List(),IPlus(Temp(AVar("r_3")),Temp(AVar("r_2")),Temp(AVar("s_2")))), 
            (18,List(),IPlus(Temp(AVar("i_3")),Temp(AVar("i_2")),IntLit(1))), 
            (19,List(),IGoto(4)), 
            (20,List(),IMove(Regstr("_r_ret"),Temp(AVar("r_2")))), 
            (21,List(),IRet))
        buildSSA(pa) match {
            case Left(err) => {
                println(err)
                assert(false)
            } 
            case Right(ssa) => assert (ssa == expected)
        }
    }
}