package sutd.compiler.simp.monad.error

import sutd.compiler.simp.monad.Functor.*
import sutd.compiler.simp.monad.Applicative.*
import sutd.compiler.simp.monad.Monad.*
import sutd.compiler.simp.monad.StateT.*
import sutd.compiler.simp.monad.error.EitherStringMonadError.{given, *}

object StateTMonadError {
    import StateT.*
    
    trait StateTMonadError[S, M[_], E] extends MonadError[StateTM[S][M], E] with StateTMonad[S, M] {
        implicit def M0:MonadError[M,E]
        override def get: StateT[S, M, S] = StateT(s => M0.pure((s, s)))
        override def set(v: S): StateT[S, M, Unit] = StateT(s => M0.pure(v, ()))

        override def raiseError[A](e: E): StateTM[S][M][A] = StateT(
            s => M0.raiseError(e)
        )
        override def handleErrorWith[A](fa: StateTM[S][M][A])(f: E => StateTM[S][M][A]): StateTM[S][M][A] = fa match {
            case StateT(run1) => 
                StateT( st => {
                    val m = run1(st)
                    M0.handleErrorWith(m)( e=> f(e) match {
                        case StateT(run2) => run2(st)
                    })}
                )
        }    
    } 


    trait StateMonadError[S] extends StateTMonadError[S, EitherM[String], String] { 
        override def M0 = eitherStringMonadError
    }
}