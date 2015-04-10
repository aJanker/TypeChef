
package de.fosd.typechef.crewrite

import de.fosd.typechef.featureexpr._
import java.io.{Writer, FileWriter, StringWriter}
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExpr
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import de.fosd.typechef.error.{Severity, TypeChefError}
import de.fosd.typechef.parser.c.SwitchStatement
import org.apache.tools.ant.util.StringUtils
import scala.collection.JavaConversions._
import java.util
import de.fosd.typechef.parser.c.FunctionDef
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.conditional.Opt

import scala.io.Source


sealed abstract class CAnalysisFrontend(tunit: TranslationUnit) extends CFGHelper with Logging {

    protected val env = CASTEnv.createASTEnv(tunit)
    protected val fdefs = filterAllASTElems[FunctionDef](tunit)
}

class CInterAnalysisFrontend(tunit: TranslationUnit, fm: FeatureModel = FeatureExprFactory.empty) extends CAnalysisFrontend(tunit) with InterCFG {

    def getTranslationUnit(): TranslationUnit = tunit

    def writeCFG(title: String, writer: CFGWriter) {
        val env = CASTEnv.createASTEnv(tunit)
        writer.writeHeader(title)

        def lookupFExpr(e: AST): FeatureExpr = e match {
            case o if env.isKnown(o) => env.featureExpr(o)
            case e: ExternalDef => externalDefFExprs.getOrElse(e, FeatureExprFactory.True)
            case _ => FeatureExprFactory.True
        }


        for (f <- fdefs) {
            writer.writeMethodGraph(getAllSucc(f, env).map {
                x => (x._1, x._2.distinct.filter { y => y.condition.isSatisfiable(fm)}) // filter duplicates and wrong succs
            }, lookupFExpr, f.declarator.getName)
        }
        writer.writeFooter()
        writer.close()

        if (writer.isInstanceOf[StringWriter])
            println(writer.toString)
    }
}

// TODO: refactoring different dataflow analyses into a composite will reduce code: handling of invalid paths, error printing ...
class CIntraAnalysisFrontendF(tunit: TranslationUnit, ts: CTypeSystemFrontend with CDeclUse, fm: FeatureModel = FeatureExprFactory.empty) extends CAnalysisFrontend(tunit) with IntraCFG {

    private lazy val udm = ts.getUseDeclMap
    private lazy val dum = ts.getDeclUseMap

    private def getDecls(key: Id): List[Id] = {
        if (! udm.containsKey(key)) List(key)
        else udm.get(key).filter { d => env.featureExpr(d) and env.featureExpr(key) isSatisfiable fm }
    }

    private val fanalyze = fdefs.map {
        x => (x, getAllSucc(x, env).filterNot {x => x._1.isInstanceOf[FunctionDef]})
    }

    var errors: List[TypeChefError] = List()
    var errNodes: List[(TypeChefError, Opt[AST])] = List()



    def deadStore(): Boolean = {
        println(fanalyze.size)
        val err = fanalyze.flatMap(deadStore)

        if (err.isEmpty) {
            println("No dead stores found!")
        } else {
            println(err.map(_._1.toString + "\n").reduce(_ + _))
        }
        errors ++= err.map(_._1)
        errNodes ++= err
        err.isEmpty
    }

    private def deadStore(fa: (FunctionDef, List[(AST, List[Opt[AST]])])): List[(TypeChefError, Opt[AST])] = {
        var err: List[(TypeChefError, Opt[AST])] = List()

        val df = new Liveness(env, udm, FeatureExprFactory.empty)

        val nss = fa._2.map(_._1)

        logger.info("Function: " + fa._1.getName + "\tAnalyse size: " + nss.size)

        for (s <- nss) {
            logger.info("Element s:" +  s)
            val killed = df.kill(s)
            logger.info("df.kill: " + killed.size)
            for ((i, fi) <- killed) {
                val out = df.out(s)

                // code such as "int a;" occurs frequently and issues an error
                // we filter them out by checking the declaration use map for usages
                if (dum.containsKey(i) && dum.get(i).size > 0) {}
                else out.find {case (t, _) => t == i} match {
                    case None => {
                        var idecls = getDecls(i)
                        if (idecls.exists(isPartOf(_, fa._1)))
                            err ::= (new TypeChefError(Severity.Warning, fi, "warning: Variable " + i.name + " is a dead store!", i, ""), Opt(env.featureExpr(i), i))
                        }
                    case Some((x, z)) => {
                        if (! z.isTautology(fm)) {
                            var xdecls = getDecls(x)
                            var idecls = getDecls(i)
                            for (ei <- idecls) {
                                // with isPartOf we reduce the number of false positives, since we only check local variables and function parameters.
                                // an assignment to a global variable might be used in another function
                                if (isPartOf(ei, fa._1) && xdecls.exists(_.eq(ei))) {
                                    err ::= (new TypeChefError(Severity.Warning, z.not(), "warning: Variable " + i.name + " is a dead store!", i, ""), Opt(env.featureExpr(i), i))
                                }
                            }
                        }
                    }
                }
            }
        }

        err
    }

    def doubleFree(): Boolean = {
        val casestudy = {
            tunit.getFile match {
                case None => ""
                case Some(x) => {
                    if (x.contains("linux")) "linux"
                    else if (x.contains("openssl")) "openssl"
                    else ""
                }
            }
        }

        val err = fanalyze.flatMap(doubleFree(_, casestudy))

        if (err.isEmpty) {
            println("No double frees found!")
        } else {
            println(err.map(_.toString + "\n").reduce(_ + _))
        }

        errors ++= err
        err.isEmpty
    }


    private def doubleFree(fa: (FunctionDef, List[(AST, List[Opt[AST]])]), casestudy: String): List[TypeChefError] = {
        var err: List[TypeChefError] = List()
        var errNode: List[AST] = List()

        val df = new DoubleFree(env, dum, udm, FeatureExprFactory.empty, casestudy)

        val nss = fa._2.map(_._1).filterNot(x => x.isInstanceOf[FunctionDef])

        for (s <- nss) {
            val g = df.gen(s)
            if (g.size > 0) {
                val in = df.in(s)

                for (((i, _), h) <- in)
                    g.find { case ((t, _), _) => t == i} match {
                        case None =>
                        case Some(((x, _), _)) => {
                            if (h.isSatisfiable(fm)) {
                                var xdecls = getDecls(x)
                                var idecls = getDecls(i)
                                for (ei <- idecls)
                                    if (xdecls.exists(_.eq(ei)))
                                        err ::= new TypeChefError(Severity.Warning, h, "warning: Variable " + x.name + " is freed multiple times!", x, "")
                            }
                        }
                    }
            }
        }
        err
    }



    def uninitializedMemory(): Boolean = {
        println(fanalyze.size)
        val err = fanalyze.flatMap(uninitializedMemory)

        if (err.isEmpty) {
            println("No usages of uninitialized memory found!")
        } else {
            println(err.map(_.toString + "\n").reduce(_ + _))
        }

        errors ++= err
        err.isEmpty
    }


    private def uninitializedMemory(fa: (FunctionDef, List[(AST, List[Opt[AST]])])): List[TypeChefError] = {
        var err: List[TypeChefError] = List()

        val um = new UninitializedMemory(env, dum, udm, FeatureExprFactory.empty)
        val nss = fa._2.map(_._1).filterNot(x => x.isInstanceOf[FunctionDef])

        println("nss size" + nss.size)

        for (s <- nss) {
            val g = um.getRelevantIdUsages(s)
            if (g.size > 0) {
                val in = um.in(s)

                for (((i, _), h) <- in)
                    g.find {case ((t, _), _) => t == i} match {
                        case None =>
                        case Some(((x, _), _)) => {
                            if (h.isSatisfiable(fm)) {
                                var xdecls = getDecls(x)
                                var idecls = getDecls(i)
                                for (ei <- idecls)
                                    if (xdecls.exists(_.eq(ei))) {
                                        err ::= new TypeChefError(Severity.Warning, h, "warning: Variable " + x.name + " is used uninitialized!", x, "")
                                        errNodes ::= (err.last, Opt(h, i))
                                    }

                            }
                        }
                    }
            }
        }

        err
    }

    def xfree(): Boolean = {
        val err = fanalyze.flatMap(xfree)

        if (err.isEmpty) {
            println("No static allocated memory is freed!")
        } else {
            println(err.map(_.toString + "\n").reduce(_ + _))
        }

        errors ++= err
        err.isEmpty
    }


    private def xfree(fa: (FunctionDef, List[(AST, List[Opt[AST]])])): List[TypeChefError] = {
        var err: List[TypeChefError] = List()

        val xf = new XFree(env, dum, udm, FeatureExprFactory.empty, "")
        val nss = fa._2.map(_._1).filterNot(x => x.isInstanceOf[FunctionDef])

        for (s <- nss) {
            val g = xf.freedVariables(s)
            if (g.size > 0) {
                val in = xf.in(s)

                for (((i, _), h) <- in)
                    g.find(_ == i) match {
                        case None =>
                        case Some(x) => {
                            if (h.isSatisfiable(fm)) {
                                val xdecls = getDecls(x)
                                var idecls = getDecls(i)
                                for (ei <- idecls)
                                    if (xdecls.exists(_.eq(ei))) {
                                        err ::= new TypeChefError(Severity.Warning, h, "warning: Variable " + x.name + " is freed although not dynamically allocted!", x, "")
                                        errNodes ::= (err.last, Opt(h, x))
                                    }
                            }
                        }
                    }
            }
        }

        err
    }

    def danglingSwitchCode(): Boolean = {
        val err = fanalyze.flatMap {x => danglingSwitchCode(x._1)}

        if (err.isEmpty) {
            println("No dangling code in switch statements found!")
        } else {
            println(err.map(_.toString + "\n").reduce(_ + _))
        }

        errors ++= err
        err.isEmpty
    }


    private def danglingSwitchCode(f: FunctionDef): List[TypeChefError] = {
        val ss = filterAllASTElems[SwitchStatement](f)
        val ds = new DanglingSwitchCode(env)

        ss.flatMap(s => {
            ds.danglingSwitchCode(s).map(e => {
                val currentError = new TypeChefError(Severity.Warning, e.feature, "warning: switch statement has dangling code ", e.entry, "")
                errNodes ::= (currentError, Opt(e.feature, e.entry))
                currentError
            })

        })
    }

    def cfgInNonVoidFunc(): Boolean = {
        val err = fanalyze.flatMap(cfgInNonVoidFunc)

        if (err.isEmpty) {
            println("Control flow in non-void functions always ends in return statements!")
        } else {
            println(err.map(_.toString + "\n").reduce(_ + _))
        }

        errors ++= err
        err.isEmpty
    }

    private def cfgInNonVoidFunc(fa: (FunctionDef, List[(AST, List[Opt[AST]])])): List[TypeChefError] = {
        val cf = new CFGInNonVoidFunc(env, ts)

        cf.cfgInNonVoidFunc(fa._1).map(
            e => {
                val currentError = new TypeChefError(Severity.Warning, e.feature, "Control flow of non-void function ends here!", e.entry, "")
                errNodes ::= (currentError, Opt(e.feature, e.entry))
                currentError
            }
        )
    }

    def caseTermination(): Boolean = {
        println(fanalyze.size)
        val err = fanalyze.flatMap(caseTermination)

        if (err.isEmpty) {
            println("Case statements with code are properly terminated with break statements!")
        } else {
            println(err.map(_.toString + "\n").reduce(_ + _))
        }

        errors ++= err
        err.isEmpty
    }

    private def caseTermination(fa: (FunctionDef, List[(AST, List[Opt[AST]])])): List[TypeChefError] = {
        val casestmts = filterAllASTElems[CaseStatement](fa._1)
        println(casestmts.size)
        val ct = new CaseTermination(env)

        casestmts.filterNot(ct.isTerminating).map {
            x => {
                val currentError = new TypeChefError(Severity.Warning, env.featureExpr(x),
                    "Case statement is not terminated by a break!", x, "")
                errNodes ::= (currentError, Opt(env.featureExpr(x), x))
                currentError
            }
        }
    }

    def stdLibFuncReturn(): Boolean = {
        val err = fanalyze.flatMap(stdLibFuncReturn)

        if (err.isEmpty) {
            println("Return values of stdlib functions are properly checked for errors!")
        } else {
            println(err.map(_.toString + "\n").reduce(_ + _))
        }

        errors ++= err
        err.isEmpty
    }

    private def stdLibFuncReturn(fa: (FunctionDef, List[(AST, List[Opt[AST]])])): List[TypeChefError] = {
        var err: List[TypeChefError] = List()
        val cl: List[StdLibFuncReturn] = List(
            //new StdLibFuncReturn_EOF(env, dum, udm, fm),

            new StdLibFuncReturn_Null(env, dum, udm, FeatureExprFactory.empty)
        )

        for ((s, _) <- fa._2) {
            for (cle <- cl) {
                lazy val errorvalues = cle.errorreturn.map(PrettyPrinter.print).mkString(" 'or' ")

                // check CFG element directly; without dataflow analysis
                for (e <- cle.checkForPotentialCalls(s)) {
                    err ::= new TypeChefError(Severity.SecurityWarning, env.featureExpr(e), "Return value of " +
                        PrettyPrinter.print(e) + " is not properly checked for (" + errorvalues + ")!", e)
                    errNodes ::= (err.last, Opt(env.featureExpr(e), e))
                }


                // stdlib call is assigned to a variable that we track with our dataflow analysis
                // we check whether used variables that hold the value of a stdlib function are killed in s,
                // if not we report an error
                val g = cle.getUsedVariables(s)
                val in = cle.in(s)
                for (((e, _), fi) <- in) {
                    println("s", PrettyPrinter.print(s), "in", in, "g", g, "u", cle.uses(s))
                    g.find {case ((t, _), _) => t == e} match {
                        case None =>
                        case Some((k@(x, _), _)) => {
                            if (fi.isSatisfiable(fm)) {
                                var xdecls = getDecls(x)
                                var edecls = getDecls(e)

                                for (ee <- edecls) {
                                    val kills = cle.kill(s)
                                    if (xdecls.exists(_.eq(ee)) && !kills.contains(k)) {
                                        err ::= new TypeChefError(Severity.SecurityWarning, fi, "The value of " +
                                            PrettyPrinter.print(e) + " is not properly checked for (" + errorvalues + ")!", e)
                                            errNodes ::= (err.last, Opt(env.featureExpr(e), e))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        err
    }

    def getInteractionDegrees(simplifyFM : java.io.File ) : (List[(Opt[Statement], Int)],
        List[(Int, TypeChefError)], List[(Opt[AST], Int, TypeChefError)]) = {

        if (simplifyFM == null) {
            def simplify(feature : BDDFeatureExpr) : BDDFeatureExpr = feature
            interactionDegrees(simplify)
        } else if (simplifyFM.getName.endsWith(".model")) {
            val simplifyModel = FeatureToSimplifyModelMap.fill(simplifyFM)
            def simplify(feature : BDDFeatureExpr) : BDDFeatureExpr = {
                val parser = new FeatureExprParser()
                val simpleFM = feature.collectDistinctFeatureObjects.foldLeft(FeatureExprFactory.True)((f, s) => {
                    simplifyModel.get(s.feature) match {
                        case Some(l) => l.foldLeft(f)(_ and parser.parse(_))
                        case _ => f
                    }
                })
                val result = feature.simplify(simpleFM).asInstanceOf[BDDFeatureExpr]
                result
            }

            interactionDegrees(simplify)
        } else {
            val parser = new FeatureExprParser()
            val simpleFM = Source.fromFile(simplifyFM).getLines.foldLeft(FeatureExprFactory.True)((f, l) => {
                if (l.matches("\\s*")) f
                else f and parser.parse(l)
            })

            def simplify(feature : BDDFeatureExpr) : BDDFeatureExpr =
                feature.simplify(simpleFM).asInstanceOf[BDDFeatureExpr]

            interactionDegrees(simplify)
        }
    }

    private def interactionDegrees(simplify : BDDFeatureExpr => BDDFeatureExpr) : (List[(Opt[Statement], Int)],
        List[(Int, TypeChefError)], List[(Opt[AST], Int, TypeChefError)]) = {
        // calculate the interaction degree of each statement itself
        val stmtsDegrees: List[(Opt[Statement], Int)] = filterAllASTElems[Statement](tunit).flatMap(stmt => {
            val stmtFExpr = env.featureExpr(stmt).asInstanceOf[BDDFeatureExpr]

            if (filterAllASTElems[Statement](stmt).size == 1)
                Some((Opt(stmtFExpr, stmt), calculateInteractionDegree(stmtFExpr, simplify)))
            else None
        })

        val warnDegrees: List[(Int, TypeChefError)] =
            ts.getASTerrors(false).filter(Set(Severity.Warning, Severity.SecurityWarning) contains _.severity).flatMap(warning => {
               val warnFEXpr = warning.condition.asInstanceOf[BDDFeatureExpr] //TODO add ast element
               Some(calculateInteractionDegree(warnFEXpr, simplify), warning)
            })

        val errDegrees : List[(Opt[AST], Int, TypeChefError)] = errNodes.flatMap(node => {
            val stmtFExpr = env.featureExpr(node._2.entry).asInstanceOf[BDDFeatureExpr]
            Some((Opt(stmtFExpr, node._2.entry), calculateInteractionDegree(stmtFExpr, simplify), node._1))
        })

        (stmtsDegrees, warnDegrees, errDegrees)
    }

    def writeWarningDegrees(warnDegrees : List[(Int, TypeChefError)], writer: Writer) = {
        warnDegrees.foreach(entry => {
            writer.write(entry._1.toString)
            writer.write(" \tFeature: ")
            writer.write(entry._2.condition.toString)

            writer.write(" \tError: ")
            writer.write(entry._2.toString)
            writer.write("\n==========\n")
        })
    }

    def writeErrorDegrees(errorDegrees: List[(Opt[AST], Int, TypeChefError)], writer: Writer) = {
        errorDegrees.foreach(entry => {
            writeDegree((entry._1, entry._2), writer)
            writer.write(" \tPriror Statement: ")
            writer.write(findPriorASTElem[Statement](entry._1.entry, env) match {
                case Some(x) => PrettyPrinter.print(x)
                case None => "Non found. "
            })
            writer.write(" \tError: ")
            writer.write(entry._3.toString)
            writer.write("\n==========\n")
        })
    }

    def writeStatementDegrees(stmtsDegrees: List[(Opt[Statement], Int)], writer: Writer) = {
        stmtsDegrees.foreach(entry => {
            writeDegree(entry, writer)
            writer.write("\n==========\n")
        })
    }

    private def writeDegree(stmtDegree: (Opt[AST], Int), writer: Writer) = {
        writer.write(stmtDegree._2.toString)
        writer.write(" \tFeature: ")
        writer.write(stmtDegree._1.feature.toString)
        writer.write(" \tStatment: ")
        writer.write(PrettyPrinter.print(stmtDegree._1.entry))
        writer.write(" \tAST: ")
        writer.write(stmtDegree._1.entry.toString)
    }

    private def calculateInteractionDegree(fExpr: BDDFeatureExpr, simplify : BDDFeatureExpr => BDDFeatureExpr): Int = {
        //interaction degree is the smallest number of variables that need to be set to reproduce the problem
        //we use the shortest path in a BDD as simple way of computing this

        //this does not always return the optimal solution, because variable ordering matters!
        //for example (fa and fb) or (fa andNot fb and fc) or (fa.not and fb) will produce a result 2 instead of 1
        val simpleFexpr = simplify(fExpr)
        val allsat = simpleFexpr.leak().allsat().asInstanceOf[util.LinkedList[Array[Byte]]]

        if (allsat.isEmpty) 0
        else allsat.map(_.count(_ >= 0)).min
    }
}
