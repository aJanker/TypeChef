package de.fosd.typechef


import java.io._
import java.time.{Duration, Instant}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import de.fosd.typechef.crewrite._
import de.fosd.typechef.error.TypeChefError
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.options.{FrontendOptions, FrontendOptionsWithConfigFiles, OptionException}
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c.{CTypeContext, TranslationUnit, _}
import de.fosd.typechef.typesystem._

object Frontend extends EnforceTreeHelper with ASTNavigation with ConditionalNavigation {

    def main(args: Array[String]) {
        // load options
        val opt = new FrontendOptionsWithConfigFiles()
        try {
            try {
                opt.parseOptions(args)
            } catch {
                case o: OptionException => if (!opt.isPrintVersion) throw o
            }

            if (opt.isPrintVersion) {
                println("TypeChef " + getVersion)
                return
            }
            if (opt.isPrintIncludes)
                opt.printInclude()
        }

        catch {
            case o: OptionException =>
                println("Invocation error: " + o.getMessage)
                println("use parameter --help for more information.")
                return
        }

        processFile(opt)
    }


    def getVersion: String = {
        var version = "development build"
        try {
            val cl = Class.forName("de.fosd.typechef.Version")
            version = "version " + cl.newInstance().asInstanceOf[VersionInfo].getVersion
        } catch {
            case e: ClassNotFoundException =>
        }
        version
    }

    private class StopWatch {
        var lastStart: Instant = Instant.now()
        var currentPeriod: String = "none"
        var currentPeriodId: Int = 0
        var times: Map[(Int, String), Long] = Map()

        private def genId(): Int = { currentPeriodId += 1; currentPeriodId }

        def start(period: String) {
            val now = Instant.now()
            val lastTime = Duration.between(lastStart, now).toMillis
            times = times + ((genId(), currentPeriod) -> lastTime)
            currentPeriod = period
            lastStart = Instant.now()
        }

        def get(period: String): Long = times.filter(v => v._1._2 == period).headOption.map(_._2).getOrElse(0)

        override def toString = {
            def nsToMs(ns : Long) = ns / 1000000
            var res = "timing "
            val switems = times.toList.filterNot(x => x._1._2 == "none" || x._1._2 == "done").sortBy(_._1._1)

            if (switems.size > 0) {
                res = res + "("
                res = res + switems.map(_._1._2).reduce(_ + ", " + _)
                res = res + ")\n"
                res = res + switems.map(_._2.toString).reduce(_ + ";" + _)
            }
            res
        }
    }


    def processFile(opt: FrontendOptions) {
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val stopWatch = new StopWatch()
        stopWatch.start("loadFM")

        val smallFM = opt.getSmallFeatureModel().and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setSmallFeatureModel(smallFM) //otherwise the lexer does not get the updated feature model with file presence conditions
        val fullFM = opt.getFullFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFullFeatureModel(fullFM) // should probably be fixed in how options are read
        if (!opt.getFilePresenceCondition.isSatisfiable(fullFM)) {
            println("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return
        }

        var ast: TranslationUnit = null
        if (opt.reuseAST && opt.parse && new File(opt.getSerializedTUnitFilename).exists()) {
            println("loading AST.")
            try {
                ast = loadSerializedAST(opt.getSerializedTUnitFilename)
                ast = prepareAST[TranslationUnit](ast)
            } catch {
                case e: Throwable => println(e.toString); e.printStackTrace(); ast = null
            }
            if (ast == null)
                println("... failed reading AST\n")
        }

        stopWatch.start("lexing")
        //no parsing if read serialized ast
        val in = if (ast == null) {
            println("#lexing")
            lex(opt)
        } else null


        if (opt.parse) {
            println("#parsing")
            stopWatch.start("parsing")

            if (ast == null) {
                //no parsing and serialization if read serialized ast
                val parserMain = new ParserMain(new CParser(smallFM))
                ast = parserMain.parserMain(in, opt, fullFM)
                ast = prepareAST[TranslationUnit](ast)
                // checkPositionInformation(ast)

                if (ast != null && opt.serializeAST) {
                    stopWatch.start("serialize")
                    serializeAST(ast, opt.getSerializedTUnitFilename)
                }

            }

            if (ast != null) {

                // some dataflow analyses require typing information
                val ts = new CTypeSystemFrontend(ast, fullFM, opt) with CTypeCache with CDeclUse

                if (opt.typecheck || opt.writeInterface || opt.typechecksa) {
                    //PrCDeclUseoductGeneration.typecheckProducts(fm,fm_ts,ast,opt,
                    //logMessage=("Time for lexing(ms): " + (t2-t1) + "\nTime for parsing(ms): " + (t3-t2) + "\n"))
                    //ProductGeneration.estimateNumberOfVariants(ast, fm_ts)

                    stopWatch.start("typechecking")
                    println("#type checking")
                    ts.checkAST(printResults = true)
                    ts.errors.map(errorXML.renderTypeError)
                }

                /** I did some experiments with the TypeChef FeatureModel of Linux, in case I need the routines again, they are saved here. */
                //Debug_FeatureModelExperiments.experiment(fm_ts)


                if (opt.writeInterface) {
                    stopWatch.start("interfaces")
                    val interface = ts.getInferredInterface().and(opt.getFilePresenceCondition)

                    stopWatch.start("writeInterfaces")
                    ts.writeInterface(interface, new File(opt.getInterfaceFilename))
                    if (opt.writeDebugInterface)
                        ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
                }
                if (opt.dumpcfg) {
                    println("#call graph")
                    stopWatch.start("dumpCFG")

                    //run without feature model, because otherwise too expensive runtimes in systems such as linux
                    val cf = new CInterAnalysisFrontend(ast /*, fm_ts*/)
                    val writer = new CFGCSVWriter(new FileWriter(new File(opt.getCCFGFilename)))
                    val dotwriter = new DotGraph(new FileWriter(new File(opt.getCCFGDotFilename)))
                    cf.writeCFG(opt.getFile, new ComposedWriter(List(dotwriter, writer)))
                }
                if (opt.staticanalyses) {
                    println("#static analysis")

                    var ctDegree, dfDegree, umDegree, xfDegree, dscDegree, cfgDegree, stdDegree, dsDegree : (List[(String, FeatureExpr, Int)], Map[Int, Int]) = (List(), Map())

                    stopWatch.start("init")
                    val sa = new CIntraAnalysisFrontendF(ast, ts.asInstanceOf[CTypeSystemFrontend with CTypeCache with CDeclUse], fullFM)
                    stopWatch.start("none")

                    if (opt.warning_case_termination) {
                        stopWatch.start("casetermination")
                        val err = sa.caseTermination()
                        stopWatch.start("none")

                        ctDegree = sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "Case statements with code are properly terminated with break statements!")
                    }

                    if (opt.warning_double_free) {
                        stopWatch.start("doublefree")
                        val err = sa.doubleFree()
                        stopWatch.start("none")

                        dfDegree = sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "No double frees found!")
                    }
                    if (opt.warning_uninitialized_memory) {
                        stopWatch.start("uninitializedmemory")
                        val err = sa.uninitializedMemory()
                        stopWatch.start("none")

                        umDegree = sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "No usages of uninitialized memory found!")
                    }

                    if (opt.warning_xfree) {
                        stopWatch.start("xfree")
                        val err = sa.xfree()
                        stopWatch.start("none")

                        xfDegree = sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "No static allocated memory is freed!")
                    }

                    if (opt.warning_dangling_switch_code) {
                        stopWatch.start("danglingswitchcode")
                        val err = sa.danglingSwitchCode()
                        stopWatch.start("none")

                        dscDegree = sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "No dangling code in switch statements found!")
                    }

                    if (opt.warning_cfg_in_non_void_func) {
                        stopWatch.start("cfginnonvoidfunc")
                        val err = sa.cfgInNonVoidFunc()
                        stopWatch.start("none")

                        cfgDegree = sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "Control flow in non-void functions always ends in return statements!")
                    }

                    if (opt.warning_stdlib_func_return) {
                        stopWatch.start("checkstdlibfuncreturn")
                        val err = sa.stdLibFuncReturn()
                        stopWatch.start("none")

                        stdDegree = sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "Return values of stdlib functions are properly checked for errors!")
                    }

                    if (opt.warning_dead_store) {
                        stopWatch.start("deadstore")
                        val err = sa.deadStore()
                        stopWatch.start("none")

                        dsDegree = sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "No dead stores found!")
                    }

                    if (opt.cfg_interaction_degree) {
                        val cfgEdgeDegrees = sa.calculateCFGEdgeDegree(opt.getSimplifyFM)
                        val writer = gzipWriter(opt.getCFGDegreeFilename)
                        cfgEdgeDegrees.foreach(cfgEdgeDegree => writer.write(cfgEdgeDegree._2 + "\t" + cfgEdgeDegree._1 + "\n"))
                        writer.close()
                    }

                    if (opt.interaction_degree) {
                        println("#calculate degrees")
                        stopWatch.start("interactiondegree")
                        val degrees = sa.getInteractionDegrees(opt.getSimplifyFM)

                        //write interaction degress
                        val stmtWriter = gzipWriter(opt.getStmtInteractionDegreeFilename)
                        val warningWriter = gzipWriter(opt.getWarningStmtInteractionDegreeFilename)
                        val errorWriter = gzipWriter(opt.getErrorStmtInteractionDegreeFilename)
                        sa.writeStatementDegrees(degrees._1, stmtWriter)
                        sa.writeWarningDegrees(degrees._2, warningWriter)
                        sa.writeErrorDegrees(degrees._3, errorWriter)
                        stmtWriter.close()
                        errorWriter.close()
                    }
                    stopWatch.start("statistics")
                    println("#TOTAL_FEATURES:\t" + filterAllSingleFeatureExpr(ast).distinct.size)
                    println("#TOTAL_NODES:\t" + countNumberOfASTElements(ast))

                    // TODO Write CleanReport
                }
            }
        }
        stopWatch.start("done")
        errorXML.write()
        if (opt.recordTiming) {
            val writer = new FileWriter(new File(opt.getStopWatchFilename))
            writer.write(stopWatch.toString)
            writer.close()
            println(stopWatch)
        }
    }

    def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = {
        val tokens = new lexer.LexerFrontend().run(opt, opt.parse)
        val in = CLexerAdapter.prepareTokens(tokens)
        in
    }

    def serializeAST(ast: AST, filename: String) {
        val fw = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(filename)))
        fw.writeObject(ast)
        fw.close()
    }

    def loadSerializedAST(filename: String): TranslationUnit = try {
        val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename))) {
            override protected def resolveClass(desc: ObjectStreamClass) = { /*println(desc);*/ super.resolveClass(desc) }
        }
        val ast = fr.readObject().asInstanceOf[TranslationUnit]
        fr.close()
        ast
    } catch {
        case e: ObjectStreamException => System.err.println("failed loading serialized AST: " + e.getMessage); null
    }

    private def countNumberOfASTElements(ast: AST): Long = {
        def countNumberOfASTElementsHelper(a: Any): Long = {
            a match {
                case l: List[_] => l.map(countNumberOfASTElementsHelper).sum
                case _: FeatureExpr => 0
                case p: Product => 1 + p.productIterator.toList.map(countNumberOfASTElementsHelper).sum
                case _ => 1
            }
        }
        countNumberOfASTElementsHelper(ast)
    }

    private def printErrors(err: List[TypeChefError], noErrorMSG: String): Unit =
        if (err.isEmpty) println(noErrorMSG)
        else println(err.map(_.toString + "\n").reduce(_ + _))

    private def gzipWriter(path: String): BufferedWriter = new BufferedWriter(new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(path)), "UTF-8"))
}
