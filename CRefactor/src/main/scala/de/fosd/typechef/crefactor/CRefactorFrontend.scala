package de.fosd.typechef.crefactor

import de.fosd.typechef.crefactor.evaluation.Stats._

import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.options.{RefactorType, FrontendOptions, OptionException, FrontendOptionsWithConfigFiles}
import de.fosd.typechef.lexer
import java.io._
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.typesystem.linker.InterfaceWriter
import de.fosd.typechef.crefactor.evaluation.{Refactor, StatsCan}
import de.fosd.typechef.crefactor.evaluation.setup.{CModuleInterfaceGenerator, Building, BuildCondition}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import de.fosd.typechef.parser.c.CTypeContext
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import de.fosd.typechef.crefactor.frontend.Editor
import javax.swing.SwingUtilities
import de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.SQLiteRefactor
import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.BusyBoxRefactor
import de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.OpenSSLRefactor
import de.fosd.typechef.crefactor.backend.CModuleInterface
import de.fosd.typechef.featureexpr.bdd.FeatureExprHelper
import de.fosd.typechef.lexer.LexerFrontend

object CRefactorFrontend extends App with InterfaceWriter with BuildCondition with Logging with EnforceTreeHelper {

    private var command: Array[String] = Array()

    private var runOpt: FrontendOptions = new FrontendOptions()

    override def main(args: Array[String]): Unit = {
        runOpt = new FrontendOptionsWithConfigFiles()

        try {
            runOpt.parseOptions(args)
        } catch {
            case o: OptionException => printInvokationErrorAndExit(o.getMessage)
        }

        if (runOpt.writeProjectInterface) writeProjectModuleInterface(runOpt)

        else if (runOpt.parse) parseOrLoadTUnitandProcess(args, true)


        println("# unique Sat calls: " + FeatureExprHelper.uniqueSatCalls)
        println("# cached Sat calls: " + FeatureExprHelper.cachedSatCalls)
        println("# all Sat calls: " + (FeatureExprHelper.uniqueSatCalls + FeatureExprHelper.cachedSatCalls))
    }

    def parseOrLoadTUnitandProcess(args: Array[String], saveArg: Boolean = false) = {
        // Current re-run hack - storing the initial arguments for parsing further files then the initial with the same arguments
        if (saveArg) command = args.foldLeft(List[String]())((args, arg) => {
            if (arg.equalsIgnoreCase(runOpt.getFile)) args
            else if (arg.equalsIgnoreCase("--refEval") || arg.equalsIgnoreCase("rename") || arg.equalsIgnoreCase("extract") || arg.equalsIgnoreCase("inline")) args
            else args :+ arg
        }).toArray

        val fm = getFM(runOpt)
        runOpt.setFeatureModel(fm)

        val tunit = getTunit(runOpt, fm)

        if (tunit == null) {
            logger.error("... failed reading AST " + runOpt.getFile + "\nExiting.")
            System.exit(-1)
        }

        processFile(tunit, fm, runOpt)
    }

    def parseOrLoadTUnit(toLoad: String): (TranslationUnit, FeatureModel) = {
        val file = {
            if (toLoad.startsWith("file")) toLoad.substring(5)
            else toLoad
        }
        logger.info("Loading file: " + file)
        val opt = new FrontendOptions()
        opt.parseOptions(file +: command.clone())

        val fm = getFM(opt)
        opt.setFeatureModel(fm) //otherwise the lexer does not get the updated feature model with file presence conditions

        val tunit = getTunit(opt, fm)

        if (tunit == null) {
            logger.error("... failed reading AST " + opt.getFile + "\nExiting.")
            System.exit(-1)
        }

        (tunit, fm)
    }


    private def getTunit(opt: FrontendOptions, fm: FeatureModel): TranslationUnit = {
        val tunit =
            if (runOpt.reuseAST && new File(opt.getSerializedTUnitFilename).exists())
                loadSerializedTUnit(opt.getSerializedTUnitFilename)
            else parseTUnit(fm, opt)
        prepareAST[TranslationUnit](tunit)
    }
    private def writeProjectModuleInterface(options: FrontendOptions) = {
        val linker: CModuleInterfaceGenerator = {
            if (options.getRefStudy.equalsIgnoreCase("busybox"))
                de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.setup.linking.BusyBoxModuleInterfaceGenerator
            else if (options.getRefStudy.equalsIgnoreCase("openssl"))
                de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.setup.OpenSSLLinkModuleInterfaceGenerator
            else if (options.getRefStudy.equalsIgnoreCase("sqlite"))
                de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.setup.SQLiteModuleInterfaceGenerator
            else null
        }

        linker.main(Array())
    }

    private def processFile(tunit: TranslationUnit, fm: FeatureModel, opt: FrontendOptions) = {
        val linkInf = {
            if (opt.refLink) new CModuleInterface(opt.getLinkingInterfaceFile)
            else null
        }

        if (opt.writeBuildCondition) writeBuildCondition(opt.getFile)

        if (opt.serializeAST) serializeTUnit(tunit, opt.getSerializedTUnitFilename)

        if (opt.writeInterface) writeInterface(tunit, fm, opt)

        if (opt.refEval) refactorEval(opt, tunit, fm, linkInf)

        if (opt.prettyPrint) prettyPrint(tunit, opt)

        if (opt.canBuild) testBuildingAndTesting(tunit, fm, opt)

        if (opt.showGui) createAndShowGui(tunit, fm, opt, linkInf)

    }

    private def getFM(opt: FrontendOptions): FeatureModel = {
        val fm = {
            if (opt.getUseDefaultPC) opt.getLexerFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
            else opt.getLexerFeatureModel.and(opt.getLocalFeatureModel)
        }

        if (opt.getUseDefaultPC && !opt.getFilePresenceCondition.isSatisfiable(fm)) {
            logger.error("file has contradictory presence condition. exiting.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            System.exit(-1)
        }

        fm
    }

    private def writeInterface(ast: AST, fm: FeatureModel, opt: FrontendOptions) {
        val ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit], fm, opt) with CTypeCache with CDeclUse
        ts.checkAST()

        val interface = {
            if (opt.getUseDefaultPC) ts.getInferredInterface().and(opt.getFilePresenceCondition)
            else ts.getInferredInterface()
        }
        ts.writeInterface(interface, new File(opt.getInterfaceFilename))
        if (opt.writeDebugInterface)
            ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
    }
    private def parseTUnit(fm: FeatureModel, opt: FrontendOptions): TranslationUnit = {
        val parsingTime = new StopClock
        val parserMain = new ParserMain(new CParser(fm))
        val tUnit = parserMain.parserMain(lex(opt), opt)

        StatsCan.addStat(opt.getFile, Parsing, parsingTime.getTime)

        tUnit
    }
    private def testBuildingAndTesting(tunit: TranslationUnit, fm: FeatureModel, opt: FrontendOptions) {
        val builder: Building = {
            if (opt.getRefStudy.equalsIgnoreCase("busybox")) de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.setup.building.Builder
            else if (opt.getRefStudy.equalsIgnoreCase("openssl")) de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.setup.Builder
            else if (opt.getRefStudy.equalsIgnoreCase("sqlite")) de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.setup.Builder
            else null
        }

        val canBuild = builder.canBuild(tunit, fm, opt.getFile)
        logger.info("Can build " + new File(opt.getFile).getName + " : " + canBuild)
    }
    private def refactorEval(opt: FrontendOptions, tunit: TranslationUnit,
                             fm: FeatureModel, linkInf: CModuleInterface) {
        val caseStudy: Refactor = {
            if (opt.getRefStudy.equalsIgnoreCase("busybox")) BusyBoxRefactor
            else if (opt.getRefStudy.equalsIgnoreCase("openssl")) OpenSSLRefactor
            else if (opt.getRefStudy.equalsIgnoreCase("sqlite")) SQLiteRefactor
            else null
        }

        opt.getRefactorType match {
            case RefactorType.RENAME => caseStudy.rename(tunit, fm, opt.getFile, linkInf)
            case RefactorType.EXTRACT => caseStudy.extract(tunit, fm, opt.getFile, linkInf)
            case RefactorType.INLINE => caseStudy.inline(tunit, fm, opt.getFile, linkInf)
            case RefactorType.NONE => println("No engine type defined")
        }
    }
    private def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = CLexerAdapter.prepareTokens(new LexerFrontend().run(opt, opt.parse))

    private def serializeTUnit(ast: AST, filename: String) {
        val fw = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(filename)))
        fw.writeObject(ast)
        fw.close()
    }

    private def loadSerializedTUnit(filename: String): TranslationUnit = {
        val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename))) {
            override protected def resolveClass(desc: ObjectStreamClass) = { /*println(desc);*/ super.resolveClass(desc) }
        }
        val ast = fr.readObject().asInstanceOf[TranslationUnit]
        fr.close()
        ast
    }

    private def prettyPrint(tunit: TranslationUnit, options: FrontendOptions) = {
        val filePath = options.getFile ++ ".pp"
        val file = new File(filePath)
        logger.info("Pretty printing to: " + file.getCanonicalPath)
        val prettyPrinted = PrettyPrinter.print(tunit).replace("definedEx", "defined")
        val writer = new FileWriter(file, false)
        writer.write(addBuildCondition(filePath, prettyPrinted))
        writer.flush()
        writer.close()
    }

    private def createAndShowGui(tunit: TranslationUnit, fm: FeatureModel, opts: FrontendOptions, linkInf: CModuleInterface) = {
        val morpheus = new Morpheus(tunit, fm, linkInf, opts.getFile)
        SwingUtilities.invokeLater(new Runnable {
            def run() {
                val editor = new Editor(morpheus)
                editor.loadFileInEditor(opts.getFile)
                editor.pack()
                editor.setVisible(true)
            }
        })
    }

    private def printInvokationErrorAndExit(message : String) = {
        println("Invocation error: " + message)
        println("use parameter --help for more information.")
        System.exit(-1)
    }
}
