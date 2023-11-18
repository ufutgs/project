package sutd.compiler.simp

import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.simp.ir.PseudoAssembly.*
import sutd.compiler.simp.ir.CFG.*
import sutd.compiler.simp.ir.DF.*

class TestDF extends funsuite.AnyFunSuite {
    import Instr.* 
    import Opr.* 
    import DomTree.*
    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 11 -> 12
      *                ^    |
      *                |    v
      *                |    6 -> 7 -> 8 -> 9 -> 10
      *                -------------------------/
      */
    test("testing buildDomTree 1") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),
           4 -> List(5), 5 -> List(6, 11), 6 -> List(7), 7 -> List(8),
           8 -> List(9), 9 -> List(10), 10 -> List(4), 11 -> List(12))
        val expected = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,List()))))))))), 
                                Node(11,List(Node(12,List())))))))))))))
        buildDomTree(g, 1) match {
            case Left(err) => {
                println(err)
                assert(false)
            }
            case Right(dt) => {
                assert(dt == expected)
            }
        }
    }


    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 13 -> 14
      *                          ^    |
      *                          |    v
      *                          |    8 -> 9 -> 10 -> 11 -> 12
      *                          ---------------------------/
      */
    test("testing buildDomTree 2") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4), 4 -> List(5), 
            5 -> List(6), 6 -> List(7), 7 -> List(8, 13), 8 -> List(9),  9 -> List(10), 
            10 -> List(11), 11 -> List(12), 12 -> List(6),  13 -> List(14))
        val expected = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,
                                                List(Node(11,
                                                    List(Node(12,List()))))))))), 
                                        Node(13,
                                            List(Node(14,List())))))))))))))))))
        buildDomTree(g, 1) match {
            case Left(err) => {
                println(err)
                assert(false)
            }
            case Right(dt) => {
                assert(dt == expected)
            }
        }
    }


    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 20 -> 21
      *                ^    |
      *                |    v
      *                |    6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 17 -> 18 -> 19 ---------\
      *                |                         ^    |                             |
      *                |                         |    v                             |
      *                |                         |    12 -> 13 -> 14 -> 15 -> 16    |
      *                |                         \----------------------------/     |
      *                \-----------------------------------------------------------/
      */
    test("testing buildDomTree 3") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),  4 -> List(5), 
        5 -> List(6, 20), 6 -> List(7), 7 -> List(8), 8 -> List(9), 
        9 -> List(10), 10 -> List(11), 11 -> List(12, 17), 12 -> List(13),
        13 -> List(14), 14 -> List(15), 15 -> List(16), 16 -> List(10),
        17 -> List(18), 18 -> List(19), 19 -> List(4), 20 -> List(21)
        )
        val expected = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,
                                                List(Node(11,
                                                    List(Node(12,
                                                        List(Node(13,
                                                            List(Node(14,
                                                                List(Node(15,
                                                                    List(Node(16,List()))))))))), 
                                                        Node(17,List(Node(18,List(Node(19,List()))))))))))))))))), 
                                Node(20,List(Node(21,List())))))))))))))
        buildDomTree(g, 1) match {
            case Left(err) => {
                println(err)
                assert(false)
            }
            case Right(dt) => {
                assert(dt == expected)
            }
        }
    }

    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 11 -> 12
      *                ^    |
      *                |    v
      *                |    6 -> 7 -> 8 -> 9 -> 10
      *                -------------------------/
      */
    test("test buildDFT 1") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),
           4 -> List(5), 5 -> List(6, 11), 6 -> List(7), 7 -> List(8),
           8 -> List(9), 9 -> List(10), 10 -> List(4), 11 -> List(12))
        val dt = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,List()))))))))), 
                                Node(11,List(Node(12,List())))))))))))))
        
        val expected = Map(1 -> List(), 2 -> List(), 3 -> List(), 4 -> List(4), 
            5 -> List(4), 6 -> List(4), 7 -> List(4), 8 -> List(4),  9 -> List(4),  10 -> List(4),  
            11 -> List(), 12 -> List() ) 
        val result = buildDFT(dt, g)
        assert(result == expected)
    }


    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 13 -> 14
      *                          ^    |
      *                          |    v
      *                          |    8 -> 9 -> 10 -> 11 -> 12
      *                          ---------------------------/
      */
    test("test buildDFT 2") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4), 4 -> List(5), 
            5 -> List(6), 6 -> List(7), 7 -> List(8, 13), 8 -> List(9),  9 -> List(10), 
            10 -> List(11), 11 -> List(12), 12 -> List(6),  13 -> List(14))
        val dt = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,
                                                List(Node(11,
                                                    List(Node(12,List()))))))))), 
                                        Node(13,
                                            List(Node(14,List())))))))))))))))))
        
        val expected = Map(1 -> List(), 2 -> List(), 3 -> List(), 4 -> List(), 5 -> List()
            , 6 -> List(6), 7 -> List(6), 8 -> List(6), 9 -> List(6), 10 -> List(6), 11 -> List(6), 12 -> List(6)
            , 13 -> List(), 14 -> List() )  
        val result = buildDFT(dt, g)
        assert(result == expected)
    }


    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 20 -> 21
      *                ^    |
      *                |    v
      *                |    6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 17 -> 18 -> 19 ---------\
      *                |                         ^    |                             |
      *                |                         |    v                             |
      *                |                         |    12 -> 13 -> 14 -> 15 -> 16    |
      *                |                         \----------------------------/     |
      *                \-----------------------------------------------------------/
      */
    test("test buildDFT 3") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),  4 -> List(5), 
        5 -> List(6, 20), 6 -> List(7), 7 -> List(8), 8 -> List(9), 
        9 -> List(10), 10 -> List(11), 11 -> List(12, 17), 12 -> List(13),
        13 -> List(14), 14 -> List(15), 15 -> List(16), 16 -> List(10),
        17 -> List(18), 18 -> List(19), 19 -> List(4), 20 -> List(21)
        )
        val dt = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,
                                                List(Node(11,
                                                    List(Node(12,
                                                        List(Node(13,
                                                            List(Node(14,
                                                                List(Node(15,
                                                                    List(Node(16,List()))))))))), 
                                                        Node(17,List(Node(18,List(Node(19,List()))))))))))))))))), 
                                Node(20,List(Node(21,List())))))))))))))
        
        val expected = Map(1 -> List(), 2 -> List(), 3 -> List(),
            4 -> List(4), 5 -> List(4), 6 -> List(4), 7 -> List(4), 8 -> List(4), 9 -> List(4),
            10 -> List(10, 4), 11 -> List(10, 4), 12 -> List(10), 13 -> List(10), 14 -> List(10), 15 -> List(10),  16 -> List(10),  
            17 -> List(4), 18 -> List(4), 19 -> List(4),
            20 -> List(), 21 -> List())
        val result = buildDFT(dt, g)
        assert(result == expected)
    }
}