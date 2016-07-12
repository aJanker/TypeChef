package de.fosd.typechef

import java.io._

import de.fosd.typechef.crewrite._
import de.fosd.typechef.featureexpr._
import de.fosd.typechef.featureexpr.sat._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._

import scala.util.Random

/**
  *
  * User: rhein
  * Date: 4/2/12
  * Time: 3:45 PM
  *
  */
object FamilyBasedVsSampleBased extends EnforceTreeHelper with ASTNavigation with CFGHelper {

    /**
      * returns: (log:String, configs: List[Pair[String,List[SimpleConfiguration] ] ])
      * log is a compilation of the log messages
      * the configs-list contains pairs of the name of the config-generation method and
      * the respective generated configs
      */
    private def buildConfigurations(tunit: TranslationUnit, ff: KnownFeatures, fm: FeatureModel,
                                    opt: FamilyBasedVsSampleBasedOptions,
                                    configdir: File, caseStudy: String): (String, List[Task]) = {
        var msg: String = ""
        var log: String = ""
        println("generating configurations.")
        var startTime: Long = 0

        var tasks: List[Task] = List()

        /** try to load tasks from existing files */
        if (configdir.exists()) {

            startTime = System.currentTimeMillis()
            println("loading tasks from serialized files")
            tasks = ConfigurationHandling.loadSerializedConfigurations(ff.features, configdir)
            msg = "Time for serialization loading: " + (System.currentTimeMillis() - startTime) + " ms\n"
            println(msg)
            log = log + msg + "\n"
        }

        /** pairwise configurations */
        if (opt.pairwise) {
            val (plog, ptasks) = ConfigurationHandling.buildConfigurationsPairwise(tunit, ff, fm, opt, configdir, caseStudy, tasks)
            log = log + plog
            tasks ++= ptasks
        } else {
            tasks = tasks.filterNot(_._1 == "pairwise")
        }

        /** code coverage - no Header files */
        if (opt.codecoverageNH) {
            val (clog, ctasks) = ConfigurationHandling.buildConfigurationsCodecoverageNH(tunit, ff, fm, configdir, caseStudy, tasks)
            log = log + clog
            tasks ++= ctasks
        } else {
            tasks = tasks.filterNot(_._1 == "coverage_noHeader")
        }

        /** code coverage - including Header files */
        if (opt.codecoverage) {
            val (clog, ctasks) = ConfigurationHandling.buildConfigurationsCodecoverage(tunit, ff, fm, configdir, caseStudy, tasks)
            log = log + clog
            tasks ++= ctasks
        } else {
            tasks = tasks.filterNot(_._1 == "coverage")
        }

        /** singleconf */
        if (opt.singleconf) {
            val (flog, ftasks) = ConfigurationHandling.buildConfigurationsSingleConf(tunit, ff, fm, opt, configdir, caseStudy, tasks)
            log = log + flog
            tasks ++= ftasks
        } else {
            tasks = tasks.filterNot(_._1 == "singleconf")
        }

        /** family */
        if (opt.family) {
            val (flog, ftasks) = ("", List(Pair("family", List(new SimpleConfiguration(ff, List(), List())))))
            log = log + flog
            tasks ++= ftasks
        } else {
            tasks = tasks.filterNot(_._1 == "family")
        }

        (log, tasks)
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

    private def initSampling(fm_scanner: FeatureModel, fm: FeatureModel, ast: TranslationUnit, ff: KnownFeatures,
                             opt: FamilyBasedVsSampleBasedOptions, logMessage: String): (String, String, List[Task]) = {
        var caseStudy = ""
        var thisFilePath: String = ""
        val fileAbsPath = new File(new File(".").getAbsolutePath, opt.getFile).toString
        if (fileAbsPath.contains("linux-2.6.33.3")) {
            thisFilePath = fileAbsPath.substring(fileAbsPath.lastIndexOf("linux-2.6.33.3"))
            caseStudy = "linux"
        } else if (fileAbsPath.contains("busybox-1.18.5")) {
            thisFilePath = fileAbsPath.substring(fileAbsPath.lastIndexOf("busybox-1.18.5"))
            caseStudy = "busybox"
        } else if (fileAbsPath.contains("openssl-1.0.1c")) {
            thisFilePath = fileAbsPath.substring(fileAbsPath.lastIndexOf("openssl-1.0.1c"))
            caseStudy = "openssl"
        } else if (fileAbsPath.contains("SQLite")) {
            thisFilePath = fileAbsPath
            caseStudy = "sqlite"
        } else {
            thisFilePath = opt.getFile
        }

        val configSerializationDir = new File(thisFilePath.substring(0, thisFilePath.length - 2))

        val (configGenLog: String, typecheckingTasks: List[Task]) =
            buildConfigurations(ast, ff, fm, opt, configSerializationDir, caseStudy)
        ConfigurationHandling.saveSerializedConfigurations(typecheckingTasks,
            ff.features, configSerializationDir, opt.getFile)
        (configGenLog, thisFilePath, typecheckingTasks)
    }

    private class StopWatch {
        var lastStart: Long = 0
        var currentPeriod: String = "none"
        var currentPeriodId: Int = 0
        var times: Map[(Int, String), Long] = Map()
        val tb = java.lang.management.ManagementFactory.getThreadMXBean
        val nstoms = 1000000

        private def genId(): Int = { currentPeriodId += 1; currentPeriodId }

        def start(period: String) {
            val now = tb.getCurrentThreadCpuTime
            val lastTime = (now - lastStart) / nstoms
            times = times + ((genId(), currentPeriod) -> lastTime)
            lastStart = now
            currentPeriod = period
        }

        def get(period: String): Long = times.find(v => v._1._2 == period).map(_._2).getOrElse(0)

        override def toString = {
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

    def checkErrorsAgainstSamplingConfigs(fm_scanner: FeatureModel, fm: FeatureModel, ast: TranslationUnit,
                                          opt: FamilyBasedVsSampleBasedOptions, logMessage: String) {
        val ff: KnownFeatures = new TUnitFeatures(ast)
        val (log, fileID, samplingTasks) = initSampling(fm_scanner, fm, ast, ff, opt, logMessage)
        val samplingTastsWithoutFamily = samplingTasks.filterNot {x => x._1 == "family"}
        println("starting error checking.")
        val sw = new StopWatch()

        sw.start("typechecking")
        val ts = new CTypeSystemFrontend(ast, fm, opt) with CTypeCache with CDeclUse
        ts.checkASTSilent
        sw.start("initsa")
        val sa = new CIntraAnalysisFrontendF(ast,
            ts.asInstanceOf[CTypeSystemFrontend with CTypeCache with CDeclUse], fm)

        val outFilePrefix: String = fileID.substring(0, fileID.length - 2)

        sw.start("doublefree")
        sa.doubleFree()
        sw.start("uninitializedmemory")
        sa.uninitializedMemory()
        sw.start("casetermination")
        sa.caseTermination()
        sw.start("xfree")
        sa.xfree()
        sw.start("danglingswitchcode")
        sa.danglingSwitchCode()
        sw.start("cfginnonvoidfunc")
        sa.cfgInNonVoidFunc()
        sw.start("checkstdlibfuncreturn")
        sa.stdLibFuncReturn()
        sw.start("deadstore")
        sa.deadStore()
        sw.start("done")

        val file: File = new File(outFilePrefix + ".errreport")
        file.getParentFile.mkdirs()
        val fw: FileWriter = new FileWriter(file)
        fw.write("File : " + fileID + "\n")
        fw.write("Features : " + ff.features.size + "\n")
        fw.write(log + "\n")

        fw.write("Potential number of data-flow errors: " + sa.errors.size + "\n\n")

        for (e <- sa.errors) fw.write(e + "\n\n")

        var caughterrorsmap = Map[String, Integer]()
        for ((name, _) <- samplingTastsWithoutFamily) caughterrorsmap += ((name, 0))


        // check for each error whether the tasklist of an sampling approach contains a configuration
        // that fulfills the error condition (using evaluate)
        for (e <- sa.errors) {
            for ((name, tasklist) <- samplingTastsWithoutFamily) {
                if (tasklist.exists {x => e.condition.evaluate(x.getTrueSet.map(_.feature))})
                    caughterrorsmap += ((name, 1 + caughterrorsmap(name)))
            }
        }

        val reslist = ("all", sa.errors.size) :: caughterrorsmap.toList.sortBy(_._1)
        fw.write(reslist.map(_._1).mkString(";") + "\n")
        fw.write(reslist.map(_._2).mkString(";") + "\n")

        println(sw.toString)

        fw.close()
    }

    def typecheckProducts(fm_scanner: FeatureModel, fm: FeatureModel, ast: TranslationUnit,
                          opt: FamilyBasedVsSampleBasedOptions, logMessage: String) {
        val ff: KnownFeatures = new TUnitFeatures(ast)
        val (configGenLog, thisFilePath, typecheckingTasks) = initSampling(fm_scanner, fm, ast, ff, opt, logMessage)
        analyzeTasks(typecheckingTasks, ast, ff, fm, opt, thisFilePath, startLog = configGenLog)
    }

    private def liveness(tunit: AST, udm: UseDeclMap, env: ASTEnv, fm: FeatureModel = FeatureExprFactory.empty) {
        val fdefs = filterAllASTElems[FunctionDef](tunit)
        fdefs.map(x => intraDataflowAnalysis(x, udm, env, fm))
    }

    private def intraDataflowAnalysis(f: FunctionDef, udm: UseDeclMap, env: ASTEnv, fm: FeatureModel) {
        if (f.stmt.innerStatements.isEmpty) return

        val pp = getAllPred(f, env)
        val li = new Liveness(env, udm, FeatureExprFactory.empty, f)

        val nss = pp.map(_._1).filterNot(x => x.isInstanceOf[FunctionDef])

        for (s <- nss) {
            li.out(s)
        }
    }

    private def analyzeTasks(tasks: List[Task], tunit: TranslationUnit, ff: KnownFeatures, fm: FeatureModel,
                             opt: FamilyBasedVsSampleBasedOptions, fileID: String, startLog: String = "") {
        val log: String = startLog
        val nstoms = 1000000
        println("start analysis.")

        // measurement
        val tb = java.lang.management.ManagementFactory.getThreadMXBean

        if (tasks.size > 0) println("start task - checking (" + tasks.size + " tasks)")
        // results (taskName, (NumConfigs, productDerivationTimes, errors, typecheckingTimes, dataflowTimes))
        var configCheckingResults: List[(String, (Int, List[Long], Int, List[Long], List[Long]))] = List()
        for ((taskDesc: String, configs: List[SimpleConfiguration]) <- tasks) {
            var configurationsWithErrors = 0
            var current_config = 0
            var productDerivationTimes: List[Long] = List()
            var tcProductTimes: List[Long] = List()
            var dfProductTimes: List[Long] = List()
            for (config <- configs) {
                current_config += 1

                // product derivation
                val productDerivationStart = tb.getCurrentThreadCpuTime
                val selectedFeatures = config.getTrueSet.map(_.feature)
                val product: TranslationUnit =
                    if (taskDesc == "family") tunit
                    else ProductDerivation.deriveProduct[TranslationUnit](tunit, selectedFeatures)

                val productDerivationDiff = tb.getCurrentThreadCpuTime - productDerivationStart
                productDerivationTimes ::= (productDerivationDiff / nstoms)
                println("checking configuration " + current_config + " of " + configs.size + " (" +
                    fileID + " , " + taskDesc + ")" + "(" + countNumberOfASTElements(product) + ")" +
                    "(" + selectedFeatures.size + ")"
                )

                val ts = if (taskDesc == "family") new CTypeSystemFrontend(product, fm) with CDeclUse
                else new CTypeSystemFrontend(product) with CDeclUse

                // typechecking measurement
                var foundError: Boolean = false
                var lastTime: Long = 0
                var curTime: Long = 0

                lastTime = tb.getCurrentThreadCpuTime
                foundError |= !ts.checkASTSilent()
                curTime = tb.getCurrentThreadCpuTime - lastTime
                val productTime: Long = curTime / nstoms

                tcProductTimes ::= productTime // append to the beginning of tcProductTimes

                // liveness measurement
                var lastTimeDf: Long = 0
                var curTimeDf: Long = 0

                lastTimeDf = tb.getCurrentThreadCpuTime
                val env = CASTEnv.createASTEnv(product)
                liveness(product, ts.getUseDeclMap, env)
                curTimeDf = tb.getCurrentThreadCpuTime - lastTimeDf
                val timeDataFlowProduct = curTimeDf / nstoms

                dfProductTimes ::= timeDataFlowProduct // add to the head - reverse later

                if (foundError) configurationsWithErrors += 1
            }
            // reverse tcProductTimes to get the ordering correct
            configCheckingResults ::=(taskDesc, (configs.size, productDerivationTimes.reverse,
                configurationsWithErrors, dfProductTimes.reverse, tcProductTimes.reverse))
        }

        val file: File = new File(fileID + "_" + tasks.map(_._1).mkString + ".vaareport")
        file.getParentFile.mkdirs()
        val fw: FileWriter = new FileWriter(file)
        fw.write("File : " + fileID + "\n")
        fw.write("Features : " + ff.features.size + "\n")
        fw.write(log + "\n")

        for ((taskDesc, (numConfigs, productDerivationTimes, errors, dfProductTimes,
        tcProductTimes)) <- configCheckingResults) {
            fw.write("\n -- Task: " + taskDesc + "\n")
            fw.write("(" + taskDesc + ")Processed configurations: " + numConfigs + "\n")
            fw.write("(" + taskDesc + ")Product Derivation Times: " + productDerivationTimes.mkString(",") + "\n")
            fw.write("(" + taskDesc + ")Configurations with errors: " + errors + "\n")
            fw.write("(" + taskDesc + ")TypecheckingSum Products: " + tcProductTimes.filter(_ > 0).sum + " ms\n")
            fw.write("(" + taskDesc + ")Typechecking Products: " + tcProductTimes.mkString(",") + "\n")
            fw.write("(" + taskDesc + ")DataflowSum Products: " + dfProductTimes.filter(_ > 0).sum + " ms\n")
            fw.write("(" + taskDesc + ")Dataflow Products: " + dfProductTimes.mkString(",") + "\n")
            fw.write("\n")
        }
        fw.close()

    }

    /**
      * This method generates and tests random configurations:
      * 1. load feature model fm
      * 2. create configuration based on selection/deselection of all features
      * 3. check whether configuration is satisfiable; increase tested
      * 3.1 if satisfiable increase valid
      * 4. repeat until timeout or after a number of tested configurations
      * 5. return pair (valid, tested)
      * @param fm input feature model (used for parsing; smaller than fmts)
      * @param fmts input feature model (used for type checking); this model can be null,
      *             since only Linux has both models
      * @return
      */
    def generateAndTestRandomConfigurations(fm: FeatureModel, fmts: FeatureModel): (Long, Long) = {
        val rndGen: Random = new Random(42)

        var tested: Long = 0
        var valid: Long = 0

        val featureMap = (if (fmts == null) fm else fmts).asInstanceOf[SATFeatureModel].variables
        val startTime = System.currentTimeMillis()
        val configsUpperBound = math.pow(2, featureMap.size)
        val numTestsMax = math.min(Int.MaxValue, configsUpperBound)

        println("start report:")
        println("|features|  : " + featureMap.size)
        println("2^|features|: " + configsUpperBound)

        while (tested < numTestsMax) {
            var enabledSet: Set[SingleFeatureExpr] = Set()
            var disabledSet: Set[SingleFeatureExpr] = Set()

            enabledSet = Set()
            disabledSet = Set()

            for ((id, key) <- featureMap) {
                if (rndGen.nextBoolean()) enabledSet += SATFeatureExprFactory.createDefinedExternal(id)
                else disabledSet += SATFeatureExprFactory.createDefinedExternal(id)
            }
            // check first fm since it is smaller the check should take less time
            // and if fexpr is not valid for fm it is not valid for fmts either
            val fexpr = SATFeatureExprFactory.createFeatureExprFast(enabledSet, disabledSet)
            if (fexpr.isSatisfiable(fm)) {
                println("fexpr maybe valid! #" + tested + " tested!")
                if (fmts != null && fexpr.isSatisfiable(fmts))
                    valid += 1
            }
            tested += 1
            if (tested % 1000 == 0) {
                println("intermediate report:")
                println("elapsed time (sec): " + ((System.currentTimeMillis() - startTime) / 1000))
                println("tested configs: " + tested + " (" + ((tested * 100) / configsUpperBound) +
                    "% of all possible)")
                println("valid configs: " + valid)
            }
        }
        println("end-of-method:")
        println("elapsed time (sec): " + ((System.currentTimeMillis() - startTime) / 1000))
        println("tested configs: " + tested + " (" + ((tested * 100) / configsUpperBound) + "% of all possible)")
        println("valid configs: " + valid)
        println("|features|: " + featureMap.size)
        println("2^|features|: " + configsUpperBound)
        (valid, tested)
    }
}