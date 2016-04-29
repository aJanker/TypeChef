package de.fosd.typechef


import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import de.fosd.typechef.Extras.StopWatch
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

                if (opt.printFeaturesPerFunction) {
                    val fw = new FileWriter(new File(opt.getFile + "-features_typechef.csv"))

                    val fa = filterAllASTElems[FunctionDef](ast)
                    if (fa.nonEmpty) fw.write("Function;Features;Names\n")

                    fa.foreach(f => {
                            val allFeatures = filterAllSingleFeatureExpr(f)
                            fw.write(f.getName + ";" + allFeatures.size + ";" + allFeatures.mkString("[", ", ", "]") + ";\n")
                        })

                    fw.close
                }

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

                    var errDegrees = Map[String, (List[(String, FeatureExpr, Int)], Map[Int, Int])]()
                    var errs = Map[String, List[TypeChefError]]()

                    stopWatch.start("init")
                    val sa = new CIntraAnalysisFrontendF(ast, ts.asInstanceOf[CTypeSystemFrontend with CTypeCache with CDeclUse], fullFM)
                    stopWatch.start("none")

                    if (opt.warning_case_termination) {
                        val analysis: String = "casetermination"
                        stopWatch.start(analysis)

                        val err = sa.caseTermination()
                        errs += analysis -> err

                        stopWatch.start("none")

                        errDegrees += analysis -> sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "Case statements with code are properly terminated with break statements!")
                    }

                    if (opt.warning_double_free) {
                        val analysis: String = "doublefree"
                        stopWatch.start(analysis)

                        val err = sa.doubleFree()
                        errs += analysis -> err

                        stopWatch.start("none")

                        errDegrees += analysis -> sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "No double frees found!")
                    }
                    if (opt.warning_uninitialized_memory) {
                        val analysis: String = "uninitializedmemory"
                        stopWatch.start(analysis)

                        val err = sa.uninitializedMemory()
                        errs += analysis -> err

                        stopWatch.start("none")

                        errDegrees += analysis -> sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "No usages of uninitialized memory found!")
                    }

                    if (opt.warning_xfree) {
                        val analysis: String = "xfree"
                        stopWatch.start(analysis)

                        val err = sa.xfree()
                        errs += analysis -> err

                        stopWatch.start("none")

                        errDegrees += analysis -> sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "No static allocated memory is freed!")
                    }

                    if (opt.warning_dangling_switch_code) {
                        val analysis: String = "danglingswitchcode"
                        stopWatch.start(analysis)

                        val err = sa.danglingSwitchCode()

                        errs += analysis -> err
                        stopWatch.start("none")

                        errDegrees += analysis -> sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "No dangling code in switch statements found!")
                    }

                    if (opt.warning_cfg_in_non_void_func) {
                        val analysis: String = "cfginnonvoidfunc"
                        stopWatch.start(analysis)

                        val err = sa.cfgInNonVoidFunc()
                        errs += analysis -> err
                        stopWatch.start("none")

                        errDegrees += analysis -> sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "Control flow in non-void functions always ends in return statements!")
                    }

                    if (opt.warning_stdlib_func_return) {
                        val analysis: String = "checkstdlibfuncreturn"
                        stopWatch.start(analysis)

                        val err = sa.stdLibFuncReturn()
                        errs += analysis -> err
                        stopWatch.start("none")

                        errDegrees += analysis -> sa.getErrorDegrees(err, opt.getSimplifyFM)
                        printErrors(err, "Return values of stdlib functions are properly checked for errors!")
                    }

                    if (opt.warning_dead_store) {
                        val analysis: String = "deadstore"
                        stopWatch.start(analysis)

                        val err = sa.deadStore()
                        errs += analysis -> err
                        stopWatch.start("none")

                        errDegrees += analysis -> sa.getErrorDegrees(err, opt.getSimplifyFM)
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

                        stopWatch.start("statistics")
                        val file: File = new File(opt.getFile + ".vaa_sumreport.gz")
                        file.getParentFile.mkdirs()

                        val fw = gzipWriter(file)
                        fw.write("[FILE]\t" + opt.getFile + "\n")
                        fw.write("[FEATURES]\t" + filterAllSingleFeatureExpr(ast).distinct.size + "\n")
                        fw.write("[NODES]\t" + countNumberOfASTElements(ast) + "\n")
                        fw.write("[DATA_FLOW_WARNINGS]\t" + sa.errors.size + "\n")
                        errs.toList.foreach(err => {
                            fw.write("[" + err._1.toUpperCase + "_VAA_DATA_FLOW_WARNINGS]\t" + err._2.size + "\n")
                            fw.write("[" + err._1.toUpperCase + "_VAA_DATA_FLOW_TIMINGS]\t" + stopWatch.get(err._1) + "\n")
                        })

                        fw.close

                        val detailReport: File = new File(opt.getFile + ".vaa_detailreport.gz")
                        detailReport.getParentFile.mkdirs()

                        val w = gzipWriter(detailReport)
                        w.write("[FILE]\t" + opt.getFile + "\n")

                        w.write("[ALLDEGREES]" + "\t")
                        w.write(sa.getAllDegrees(opt.getSimplifyFM).mkString("; ") + "\n")

                        w.write("[ALLWARNINGDEGREES]" + "\t")
                        w.write(sa.getErrorDegrees(sa.errors, opt.getSimplifyFM)._2.mkString("; ") + "\n")

                        errDegrees.foreach(err => {
                            w.write("[" + err._1.toUpperCase + "_DEGREE]" + "\t" + err._2._2.mkString("; ") + "\n")
                        })

                        errDegrees.foreach(err => err._2._1.foreach(detail => {
                            w.write("[" + err._1.toUpperCase + "_DEGREE_DETAIL]" + "\t" + detail._1 + "\t" + detail._2 + "\t" + detail._3 + "\n")
                        }))

                        w.close
                    }




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

    private def gzipWriter(path: File): BufferedWriter = new BufferedWriter(new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(path)), "UTF-8"))
}
