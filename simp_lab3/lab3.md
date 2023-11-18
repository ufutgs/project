# 50.054 Project Lab 3 (15%)

## Deadline - 10 Dec 2023 23:59

In the previous lab, we completed the big step operational semantics oand type inference for SIMP program.

If you have trouble completing lab 2, you can send the instructor an email to ask for the sample solution. 

In this lab, we look into the name analysis, liveness analysis and target code generation of the SIMP language.

## Tasks for this Lab

The three main tasks for this lab include

1. Implementing the algorithm that constructs an SSA form from a Psuedo Assembly program.
1. Implementing the liveness analysis for Psuedo Assembly Programs
1. Implementing the JVM bytecode generator for Psuedo Assembly Programs 

There is no obvious dependency between the two tasks, hence you can divide the task among your team members if you wish to.


## Prelude 

To migrate from your lab 1 and lab 2 answers to this project, 
You should copy the `.scala` files that have been modified by you and overwrite those counterparts in this project template, (the ones being overwritten should be the same as those found in the lab 1 and lab 2 project template.)


The given project template contains the following new files/folders comparted to lab 1 and lab 2 project template.

* `src/main/scala/sutd/compiler/simp/backend` - the folder for the JVM bytecode backend
* `src/main/scala/sutd/compiler/simp/ir` - contains the following new files
    * `CFG.scala` - the CFG construction module
    * `DF.scala` - the Dominance Frontier construction module
    * `SSA.scala` - the SSA construction module
* `src/main/scala/sutd/compiler/simp/lattice/` - contains the modules for Lattice operations
* `src/main/scala/sutd/compiler/simp/monad/error/` - contains the additional monad error instances
* `src/main/scala/sutd/compiler/simp/semantic/` - contains the following new files
    * `LivenessAnalysis.scala`  the liveness analysis module (which should be completed in the cohort exercise in week 11)
    * `SignAnalysis.scala` - the sign analysis module
    * `Util.scala` - some utility functions for semantic analysis (and code generation)
* `src/main/scala/Main.scala` - has been modified to accommodate the compiler flag and code generation integration.
* `src/test/scala/sutd/compiler/simp/` - contains the following new test suites
    * `TestCFG.scala`
    * `TestDF.scala`
    * `TestLivenessAnalysis.scala`
    * `TestSignAnalysis.scala`
    * `TestSSA.scala`
* `build.sbt` - has been modified to include the `org.ow2.asm` external dependency which is required for JVM bytecode manipulation.
* `project/plugins.sbt`- is a newly added file, to support the packaging of `org.ow2.asm` dependencies and Scala run-time into a single jar file.


## Task 1 : Constructing SSA (5 marks)

In this task we look into constructing the Static Single Assignment from given a Psuedo Assembly program. 

Recall the flow of constructing of SSA goes as follows

```mermaid 
graph TD;
    PA -->  CFG --> DomTree --> DFTable 
    CFG --> DFTable
    DFTable ---> E["DF+/E lookup table"]
    PA --> E
    PA --> PreSSA["SSA w/o variable renaming"]
    E  --> PreSSA
    PreSSA --> SSA["SSA w/ variables renamed"]
```

The construction of CFG from PA `CFG.scala` has been implemented, you don't have to update it.
The construction of DomTree from CFG has been given in `DF.scala`

### Sub Task 1.1 

In this sub task, you are supposed to complete the implementation dominance frontier table construction in `DF.scala`. Recall the steps are as follows,

1. traverse the dominator tree in post-order
1. for each vertex, compute the $df_{local}$ and its children's $df_{up}$
    1. union the two components and save it to the table.

You may find the following data structures and functions useful.

* `CFG.scala`
    1. `CFG`
    1. `successors`
* `DF.scala`
    1. `DomTree`
    1. `childOf`
    1. `isChildOf`
    1. `postOrderTrav`

After this sub task, you should be able to pass all the test cases in `TestDF.scala`

### Sub Task 1.2 

In this sub task, you are supposed to complete the implementation of SSA construction in `SSA.scala`.

According to the notes (Cytron's algorithm), there are two steps, 

1. building a Pre-SSA form where phi assignments are inserted to the $DF^+$. 
1. renaming the variables in the a Pre-SSA form.

Step 2 has been implemeneted. Step 1 is incomplete. Fill in the missing parts in `SSA.scala` to complete this step.


You may find the following data structures and functions useful.

* `CFG.scala`
    1. `CFG`
    1. `predecessors`
* `DF.scala`
    1. `DFplus`
    1. `PhiAssignment`


When you are done, you should be able to pass all the test cases in `TestSSA.scala`



## Task 2 : Liveness Analysis (5 marks)

Recall that liveness analysis aims to discover the set of program variables that may "live", for each program location $l$. 

The idea is 

1. to define the abstract domain of the solution, then
1. to show that the abstract values (as well as the abstract states) are elements from some copmlete lattice.
1. to generate an monotonic equation system (one equation per program location)
    1. each equation defines the relationship between abstract values (from different abstract states, i.e. those arised from adjacent program locations)
    1. re-express the monotonic equation system into a monotone function
1. to apply fixed point algorithm to solve the equation system

For liveness analysis, the abstract states are sets of program variables at each program locations. It is a power set lattice, hence it is a complete lattice. 

The definition of a complete lattice is given as type class `CompleteLattice` (in `CompleteLattice.scala`). The definition of a powerset lattice is given as a type class instance of `CopleteLattice`.


### Sub Task 2.1 

In this task you need to complete the definition of the helper function `join()` in `LivenessAnalysis.scala` to *merge* state objects from the previous states. (Hint, recall that liveness analysis is a backward analysis.) 


### Sub Task 2.2 
In addition, you need to complete the definition of the monotone function, `genMonotonFunction`. Note that the type of monotone function in livenesss analysis is `AbstractEnv => AbstractEnv`, where `AbstractEnv` is a `Map[Label, AbstractState]`. By definition, `AbstractEnv` forms a map lattice.  In this implementation, we generalize the type of the monoetone function to `AbstractEnv => Either[String, AbstractEnv]` to handle error reporting. 

When you are done, you should be able to pass all the test cases in `TestLivenessAnalysis.scala`


## Task 3 : Generating JVM Bytecodes (3 marks)

Though this task can be completed without the first 2 tasks, you are advised to finish the other two tasks before attempting this task. 

In this task, you are required to complete the partially implemented function `convertInstr()`  in `backend/JVM.scala` which emits the JVM bytecode given a PA labeled instruction.

You may find the following function and data structure useful.


* `monad/error/StateTMonadError.scala`
* `monad/error/EitherStringMonadError.scala`
* `JVM.scala`
    1. `convertLabel`
    1. `convertOpr`
    1. `getMV`
* JVM bytecode documentation 
    1. `https://asm.ow2.io/asm4-guide.pdf`  (esp, Appendix A.1.)


There is no test suite defined in scalatest.

When you are done, run

```bash
$ sbt compile
$ sbt test
```
to compile. Then run 

```bash
$ sbt assembly
```
to generate  a mega jar (with all the jvm bytecode depenedencies and Scala run-time library)

To test 

```bash
$ scala jar target/scala-3.3.0/simp_all.jar -c examples/fib.simp 
```
which prints `Compilation done.` and generate a JVM byte file, `GeneratedClass.class`

```bash 
$ java GeneratedClass 5 
```
which should produce `8`.

You can try and test with some other `.simp` programs in `examples` or your own creation.

Congratulations! You have completed this project! 
Now you can `sbt clean` and zip up your project and submit.


## Task 4: Mastering Scala and Compiler Design (2 marks) 

Note for this task, there will be a seperate submission entry. 

You are supposed to implement an extension to the simp language compiler. 

You are free to choose what extension you want to implement. Some of the following are possible suggestion.

Whatever you decide to implement, you can submit your (semi- or fully) working prototype along with a not-more-than 2000-word report, it can be in PDF, word doc or markdown.

### Better error reporting 

Note that there is a `SrcLoc.scala` module being used in the lexer. It keeps track of the current line and column of the parsed/lexed token. In case of error, we can give a more informative message with the source program location. Can you extend the parser, type inference, and other possible analysese to incorporate the source locations. 


### Liveness Analysis for SSA

Note that currently the liveness analysis is performed on the level of PA, but not on SSA. In the lecture we discussed how this can be extended to SSA. You can try to extend the liveness analysis to SSA PA.


### Pretty Printer for PA

Some times debugging PA is a chore. It would be good to have a pretty printer to convert a PA AST into a nicely formatted string. For example 

```scala
val input = Temp(AVar("input"))
val x = Temp(AVar("x"))
val y = Temp(AVar("y"))
val z = Temp(AVar("z"))
val w = Temp(AVar("w"))
val r_ret = Regstr("_r_ret")
val pa = List(
    (1, IMove(x, input)),
    (2, IPlus(y, x, IntLit(1))),
    (3, IPlus(z, y, IntLit(1))),
    (4, IMult(w, y, z)),
    (5, IMove(r_ret, w)),
    (6, IRet)
)
```
can be pretty-printed as 

```java
1: x <- inpput
2: y <- x + 1
3: z <- y + 1
4: w <- y * z
5: _r_ret <- w
6: ret
```

### Register allocation 

We can apply what we learned in week 12 content to apply the register allocation to a PA SSA. We can simulate it by returning a PA with all operation temporary variable operands replaced by registeres.