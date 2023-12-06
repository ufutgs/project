package sutd.compiler.simp.lattice

import sutd.compiler.simp.lattice.CompleteLattice.{given, *} 

object SignLattice {
    import CompleteLattice.* 
    enum SignAbsVal {
        case Bot    // _|_
        case Minus  // -
        case Plus   // + 
        case Top    // T
        case Zero   // 0
    }

    import SignAbsVal.*
    given signLattice:CompleteLattice[SignAbsVal] = new CompleteLattice[SignAbsVal] {
        def sqSubSetEq(a: SignAbsVal, b: SignAbsVal): Option[Boolean] = (a,b) match {
            case (Bot, _)   => Some(true) 
            case (Top, Top) => Some(true)
            case (Top, _)   => Some(false)
            case (Plus, Bot) => Some(false)
            case (Plus, Plus) => Some(true)
            case (Plus, Top)  => Some(true)
            case (Plus, _)    => None
            case (Minus, Bot) => Some(false)
            case (Minus, Minus) => Some(true)
            case (Minus, Top)   => Some(true)
            case (Minus, _)     => None
            case (Zero, Bot) => Some(false)
            case (Zero, Zero) => Some(true)
            case (Zero, Top)  => Some(true)
            case (Zero, _)    => None
        }
        def lub(a:SignAbsVal, b:SignAbsVal):SignAbsVal = (a,b) match {
            case (Bot,_)        => b 
            case (Top,_)        => Top
            case (_, Bot)       => a 
            case (_, Top)       => Top 
            case (Plus, Plus)   => Plus 
            case (Plus, _)      => Top
            case (Minus, Minus) => Minus
            case (Minus, _)     => Top 
            case (Zero, Zero)   => Zero
            case (Zero, _)      => Top 
        }
    }
}