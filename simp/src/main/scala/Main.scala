package sutd.compiler.simp

import scala.io.Source
import sutd.compiler.simp.syntax.Lexer.*
import sutd.compiler.simp.syntax.Parser.*
import sutd.compiler.simp.syntax.Parsec.*
import sutd.compiler.simp.syntax.Parsec
import sutd.compiler.simp.syntax.Parsec.Progress
import sutd.compiler.simp.syntax.AST.*
import sutd.compiler.simp.semantic.TypeInf.{given, *}
import sutd.compiler.simp.interpreter.SimpInt.*

object Main {
    def main(args:Array[String]):Unit = {
        if (args.length < 3) {
            println("""
            USAGE: scala jar simp.jar <mode> source.simp [inputs]
              e.g. scala jar simp.jar -i fib.simp 2
            """)
        } else {
            val flag = args(1)
            val simpfile = args(2)
            if ((flag == "-i") && (args.length < 4)) {
                println("""
                USAGE: scala jar simp.jar -i source.simp <input>
                e.g. scala jar simp.jar -i fib.simp 2
                """)
            } else {
                val input = args(3).toInt
                exec(simpfile, input) match {
                    case Left(error) => {
                        println("Error:")
                        println(error)
                    } 
                    case Right(result) => {
                        println(result)
                    }
                }
            }
        }
    }
    import Progress.*
    import Result.*
    import Const.*
    def exec(filename:String, input:Int):Either[String, String] = {
        try {
            val lines = Source.fromFile(filename).getLines.toList.mkString("\n")
            Parsec.run(lex)(LEnv(lines.toList, 1, 1)) match {
                case Consumed(Ok((toks, lenv))) if eof(lenv) => {
                    Parsec.run(parse)(PEnv(toks)) match {
                        case Consumed(Ok((stmts, penv))) if done(penv) => {
                            typeInf(stmts) match {
                                case Left(errorMessage) => {
                                    Left(errorMessage)
                                }
                                case Right(typeenv) => {
                                    interpret(stmts,input) match {
                                        case Left(err) => Left(err)
                                        case Right(IntConst(result)) => Right(result.toString())
                                        case Right(BoolConst(result)) => Right(result.toString())
                                    }
                                }
                            }
                        }
                        case Consumed(Ok((stmts, penv)))  => {
                            Left(s"Parser terminated prematurely at ${penv._1}")
                        }
                        case Consumed(Failed(msg)) => 
                            Left(msg)
                        case Empty(Ok((stmts, penv))) if done(penv) => {
                            Left("An empty file is given.")
                        }
                        case Empty(Ok((stmts, penv)))  => {
                            Left(s"Parser terminated prematurely at ${penv._1}")
                        }
                        case Empty(Failed(msg)) => 
                            Left(msg)
                    }
                }
                case Consumed(Ok((toks, lenv))) => {
                    Left(s"Lexer terminated prematurely at ${lenv._1.mkString("")}")
                }
                case Consumed(Failed(msg)) => Left(msg)
                case Empty(Ok((toks, lenv))) if eof(lenv) => {
                    Left("An empty file is given.")
                }
                case Empty(Ok((toks, lenv))) => {
                    Left(s"Lexer terminated prematurely at ${lenv._1.mkString("")}")
                }
                case Empty(Failed(msg)) => Left(msg)
            }
            
        } catch {
            case e: java.io.FileNotFoundException => Left(s"File ${filename} does not exists.")
        }
    }
}