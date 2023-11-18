package sutd.compiler.simp



import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.simp.ir.PseudoAssembly.*
import sutd.compiler.simp.ir.CFG.*

class TestCFG extends funsuite.AnyFunSuite {
    import Instr.* 
    import Opr.* 
    test("testing buildCFG 1") {
        /* 1: x <- 0
           2: y <- 10
           3: i <- 0
           4: var_1 <- i < y
           5: ifn var_1 goto 11
           6: var_2 <- x + i
           7: x <- var_2
           8: var_3 <- i + 1
           9: i <- var_3
           10: goto 4
           11: r_ret <- x
           12: ret
        */
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
        val expected = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),
           4 -> List(5), 5 -> List(6, 11), 6 -> List(7), 7 -> List(8),
           8 -> List(9), 9 -> List(10), 10 -> List(4), 11 -> List(12))
        val cfg = buildCFG(pa)
        assert (cfg == expected)
    }


    test("testing buildCFG 2") {
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
        val expected = Map(1 -> List(2), 2 -> List(3), 3 -> List(4), 4 -> List(5), 
            5 -> List(6), 6 -> List(7), 7 -> List(8, 13), 8 -> List(9),  9 -> List(10), 
            10 -> List(11), 11 -> List(12), 12 -> List(6),  13 -> List(14))
        val cfg = buildCFG(pa)
        assert (cfg == expected)
    }



    test("testing buildCFG 3") {
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
        val expected = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),  4 -> List(5), 
        5 -> List(6, 20), 6 -> List(7), 7 -> List(8), 8 -> List(9), 
        9 -> List(10), 10 -> List(11), 11 -> List(12, 17), 12 -> List(13),
        13 -> List(14), 14 -> List(15), 15 -> List(16), 16 -> List(10),
        17 -> List(18), 18 -> List(19), 19 -> List(4), 20 -> List(21)
        )
        val cfg = buildCFG(pa)
        assert (cfg == expected)
    }
}