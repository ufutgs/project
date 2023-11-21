file:///C:/Users/user/Desktop/project/simp/src/main/scala/sutd/compiler/simp/syntax/Parser.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
offset: 5682
uri: file:///C:/Users/user/Desktop/project/simp/src/main/scala/sutd/compiler/simp/syntax/Parser.scala
text:
```scala
package sutd.compiler.simp.syntax

import sutd.compiler.simp.syntax.Lexer.*
import sutd.compiler.simp.syntax.SrcLoc.*
import sutd.compiler.simp.syntax.AST.*
import sutd.compiler.simp.syntax.Parsec.*
import org.scalactic.Bool

object Parser {
    /**
     * S ::= X = E ; | return X ; | nop | if E { \overline{S} } else { \overline{S} } | while E { \overline{S} } 
     * E ::= E Op E | X | C | (E)
     * \overline{S} ::= S | S \overline{S}
     * Op ::= + | - | *  
     * C ::= 1 | 2 | ... | true | false 
     * X ::= a | b | c | d 
     * */


    import Stmt.*
    import Exp.*
    import Const.* 
    
    import LToken.*
    import Progress.*
    import Result.*

    case class PEnv(toks: List[LToken])

    /**
      * check whether the parsing is done based on the list of tokens left.
      *
      * @param env
      * @return boolean
      */
    def done(env:PEnv):Boolean = env match {
        case PEnv(Nil) => true
        case _ => false
    }

    /**
      * type class instance of ParserEnv[PEnv, LToken]
      */
    given penvParserEnv: ParserEnv[PEnv, LToken] = new ParserEnv[PEnv, LToken] {
        override def getTokens(env: PEnv): List[LToken] = env match {
            case PEnv(toks) => toks
        }
        override def getCol(env: PEnv): Int = env match {
            case PEnv(Nil) => -1
            case PEnv(tok :: toks) =>
                srcLoc(tok) match {
                    case SrcLoc(ln, col) => col
                }
        }
        override def getLine(env: PEnv): Int = env match {
            case PEnv(Nil) => -1
            case PEnv(tok :: toks) =>
                srcLoc(tok) match {
                    case SrcLoc(ln, col) => ln
                }
        }
        override def setTokens(ts: List[LToken])(env: PEnv): PEnv = env match {
            case PEnv(_) => PEnv(ts)
        }

    }
    /**
      * The top level parser
      */
    def parse:Parser[PEnv, List[Stmt]] = p_stmts

    /**
      * Parsing a sequence of statements, 
      * we skip the preceeding and the proceeding white spaces for each statement.
      * for individual statement parser, 
      * we only need to skip the whitespace in between.
      *
      * @return
      */
    def p_stmts:Parser[PEnv, List[Stmt]] = {
        def p_one:Parser[PEnv, Stmt] = for {
            _ <- p_spaces
            s <- p_stmt
            _ <- p_spaces
        } yield s
        many(p_one)
    } 

    /**
      * Parsing a statement
      *
      * @return
      */
    def p_stmt:Parser[PEnv, Stmt] = choice(p_assign)(choice(p_ret)(choice(p_nop)(choice(p_ifelse)(p_while))))

    /**
      * Parsing a Nop statement
      *
      * @return
      */
    def p_nop:Parser[PEnv, Stmt] = for {
        _ <- sat((tok:LToken) => tok match {
            case NopKW(src) => true 
            case _ => false 
        })
        _ <- p_spaces
        _ <- p_semicolon
    } yield Nop

    /**
      * Parsing an assignment statement
      *
      * @return
      */
    def p_assign:Parser[PEnv, Stmt] = for {
        x <- p_var
        _ <- p_spaces
        _ <- p_equal
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_semicolon
    } yield Assign(x, e)

    /**
      * Parsing a return statement
      *
      * @return
      */
    def p_ret:Parser[PEnv, Stmt] = for {
        _ <- p_returnKW
        _ <- p_spaces
        x <- p_var
        _ <- p_spaces
        _ <- p_semicolon
    } yield Ret(x)

    /**
      * Parsing an if-else statement
      *
      * @return
      */
    def p_ifelse:Parser[PEnv, Stmt] = for {
        _ <- p_ifKW
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_lbrace
        s1 <- p_stmts
        _ <- p_rbrace
        _ <- p_spaces 
        _ <- p_elseKW
        _ <- p_spaces
        _ <- p_lbrace
        s2 <- p_stmts
        _ <- p_rbrace
    } yield If(e, s1, s2)

    /**
      * Parsing a while statement
      *
      * @return
      */
    def p_while:Parser[PEnv, Stmt] = for {
        _ <- p_whileKW
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_lbrace
        s <- p_stmts
        _ <- p_rbrace
    } yield While(e, s)

    /** Lab 1 Task 1.1
      * parsing / skipping whitespaces
      *
      * @return
      */

    def p_space:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case WhiteSpace(_,_) => true
        case _ => false
    })
    
    def p_spaces:Parser[PEnv, List[LToken]] = many(p_space) 

    /** Lab 1 Task 1.1 end */


    /** Lab 1 Task 1.2 
      * Parsing an expression
      * Note that 
      *   E ::= E Op E | X | C | (E) contains left recursion
      * @return
      */
    def p_exp:Parser[PEnv, Exp] = choice(p_varToExp)(choice(p_varToConst)(p_varToParen))
    
    def p_varToExp: Parser[PEnv, Exp] = for{
        x <- p_var
        p_e <- p_Eprime
    } yield from_pVar(Pre_E.pVar(x,p_e))

    def p_varToConst: Parser[PEnv, Exp] = for{
        c <- p_const
        p_e <- p_Eprime
    }yield from_pConst(Pre_E.pConst(c,p_e))

    def p_varToParen: Parser[PEnv, Exp] = for {
        lparen <- p_lparen
        e <- p_exp
        rparen <- p_rparen
        p_e <- p_Eprime
    } yield from_pParen(Pre_E.pParen(e,p_e))

    def p_Eprime :Parser[PEnv, E_prime] = empty(E_prime.Eps) // fixme
    
    def from_pVar(e:Pre_E.pVar) :Exp = e match{
        case Pre_E.pVar(x, ep) => x match {
            case Var(y) => from_Eprime(VarExp(x))(ep)
            case Const()@@
        }
        
    }
    def from_Eprime (e:Exp) (ep: E_prime) : Exp = {

    }
    def from_pConst(e:Pre_E.pConst) :Exp = ConstExp(IntConst(1)) // fixme
    def from_pParen(e:Pre_E.pParen) :Exp = ConstExp(IntConst(1)) // fixme
    
    enum Pre_E {
        case pVar(x:Var,ep:E_prime)
        case pConst(c:Const,ep:E_prime)
        case pParen(e1:Exp,ep:E_prime)
    }
    enum E_prime{
        case  Eps
        case Plus(e1:E_prime, e2:Exp)
        case Minus(e1:E_prime, e2:Exp)
        case Mult(e1:E_prime, e2:Exp)
        case DEqual(e1:E_prime, e2:Exp)
        case LThan(e1:E_prime, e2:Exp) 
    }
    

    /** Lab 1 Task 1.2 end */
    
    /**
      * Parsing operator symbols
      *
      * @return
      */
    def p_plus:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case PlusSign(_) => true 
        case _ => false
    })

    def p_minus:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case MinusSign(_) => true 
        case _ => false
    })

    def p_mult:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case AsterixSign(_) => true 
        case _ => false
    })

    def p_lthan:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case LThanSign(_) => true 
        case _ => false
    })

    def p_dequal:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case DEqSign(_) => true 
        case _ => false
    })

    def p_equal:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case EqSign(_) => true 
        case _ => false
    })

    /**
      * Parsing a Variable
      *
      * @return
      */
    def p_var:Parser[PEnv, Var] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case IdTok(src, v) => true
            case _ => false 
        })
        name <- someOrFail(tok)( t => t match {
            case IdTok(src, v) =>  Some(v)
            case _ => None
        })("error: expecting an identifier, but None is returned.") // this error should never occur.
    } yield Var(name)


    /**
      * Parsing a Constant
      *
      * @return
      */
    def p_const:Parser[PEnv, Const] = choice(choice(p_true)(p_false))(p_int)

    def p_true:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case TrueKW(src) => true
            case _ => false 
        })
    } yield BoolConst(true)

    def p_false:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case FalseKW(src) => true
            case _ => false 
        })
    } yield BoolConst(false)

    def p_int:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case IntTok(src, v) => true
            case _ => false
        })
        i <- someOrFail(tok)( t => t match {
            case IntTok(src, v) =>  Some(v)
            case _ => None
        })("error: expecting an integer, but None is returned.") // this error should never occur.
    } yield IntConst(i)

    /**
      * Parsing keywords
      *
      * @return
      */
    def p_returnKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case RetKW(src) => true
        case _ => false
    })

    def p_ifKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case IfKW(src) => true
        case _ => false
    })

    def p_elseKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case ElseKW(src) => true
        case _ => false
    })

    def p_whileKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case WhileKW(src) => true
        case _ => false
    })



    /**
      * Parsing symbols
      */
    def p_lbrace:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case LBrace(src) => true
        case _ => false
    })

    def p_rbrace:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case RBrace(src) => true
        case _ => false
    })


    def p_lparen:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case LParen(src) => true
        case _ => false
    })

    def p_rparen:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case RParen(src) => true
        case _ => false
    })

    def p_semicolon:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case SemiColon(src) => true
        case _ => false
    })



}
```



#### Error stacktrace:

```
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2576)
	scala.meta.internal.pc.SignatureHelpProvider$.isValid(SignatureHelpProvider.scala:83)
	scala.meta.internal.pc.SignatureHelpProvider$.notCurrentApply(SignatureHelpProvider.scala:92)
	scala.meta.internal.pc.SignatureHelpProvider$.$anonfun$1(SignatureHelpProvider.scala:48)
	scala.collection.StrictOptimizedLinearSeqOps.loop$3(LinearSeq.scala:280)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile(LinearSeq.scala:282)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile$(LinearSeq.scala:278)
	scala.collection.immutable.List.dropWhile(List.scala:79)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:48)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:375)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner