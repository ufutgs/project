file:///C:/Users/user/Desktop/project/simp_lab3/src/main/scala/sutd/compiler/simp/util/PrettyPrinter.scala
### java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/user/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.10/scala-library-2.13.10-sources.jar!/scala/collection/immutable/List.scala

occurred in the presentation compiler.

action parameters:
offset: 190
uri: file:///C:/Users/user/Desktop/project/simp_lab3/src/main/scala/sutd/compiler/simp/util/PrettyPrinter.scala
text:
```scala
package sutd.compiler.simp.util
import sutd.compiler.simp.ir.PseudoAssembly.*
import Instr.*
import Opr.* 

object PrettyPrinter {
    
  def prettyPrint(pa:List[LabeledInstr]) : Unit = pa m@@

  def translate(pa:List[LabeledInstr]) : List[String] = pa.foldRight(List[String]())( (li : LabeledInstr,program:List[String]) =>li match{
    case (lnum,instr) => {
        val text = lnum.toString() +": "
        translate_Instr(text,instr) :: program
    }
  })

  def translate_Instr(text:String, li:Instr) : String = li match {
    case IMove(dest, src) =>text + translate_Opr(dest) + " <- " + translate_Opr(src)
    case IRet => text + " ret"
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

```



#### Error stacktrace:

```
java.base/sun.nio.fs.WindowsPathParser.normalize(WindowsPathParser.java:182)
	java.base/sun.nio.fs.WindowsPathParser.parse(WindowsPathParser.java:153)
	java.base/sun.nio.fs.WindowsPathParser.parse(WindowsPathParser.java:77)
	java.base/sun.nio.fs.WindowsPath.parse(WindowsPath.java:92)
	java.base/sun.nio.fs.WindowsFileSystem.getPath(WindowsFileSystem.java:232)
	java.base/java.nio.file.Path.of(Path.java:147)
	java.base/java.nio.file.Paths.get(Paths.java:69)
	scala.meta.io.AbsolutePath$.apply(AbsolutePath.scala:60)
	scala.meta.internal.metals.MetalsSymbolSearch.$anonfun$definitionSourceToplevels$2(MetalsSymbolSearch.scala:62)
	scala.Option.map(Option.scala:242)
	scala.meta.internal.metals.MetalsSymbolSearch.definitionSourceToplevels(MetalsSymbolSearch.scala:61)
	scala.meta.internal.pc.completions.CaseKeywordCompletion$.sortSubclasses(MatchCaseCompletions.scala:306)
	scala.meta.internal.pc.completions.CaseKeywordCompletion$.matchContribute(MatchCaseCompletions.scala:254)
	scala.meta.internal.pc.completions.Completions.advancedCompletions(Completions.scala:375)
	scala.meta.internal.pc.completions.Completions.completions(Completions.scala:183)
	scala.meta.internal.pc.completions.CompletionProvider.completions(CompletionProvider.scala:86)
	scala.meta.internal.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:123)
```
#### Short summary: 

java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/user/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.10/scala-library-2.13.10-sources.jar!/scala/collection/immutable/List.scala