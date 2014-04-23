package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.Evaluation
import de.fosd.typechef.parser.c.{ConditionalNavigation, ASTNavigation}
import java.io.File
import scala.io.Source
import de.fosd.typechef.featureexpr.FeatureExprFactory
import java.util.IdentityHashMap
import java.util


trait OpenSSLEvaluation extends Evaluation with ASTNavigation with ConditionalNavigation {

    val evalName = "openssl"
    val caseStudyPath = "../cRefactor-OpenSSLEvaluation/"
    val completePath = new File(caseStudyPath).getCanonicalPath
    val filesToEval: String = completePath + "/openssl_files"
    val evalFiles = getEvaluationFiles
    val blackListFiles: List[String] = List()
    val blackListIds: List[String] = List()
    //Source.fromFile(getClass.getResource("/openssl_id_blacklist").getFile).getLines().toList
    //Source.fromFile(getClass.getResource("/openssl_blacklist").getFile).getLines().toList
    val sourcePath = completePath + "/" + evalName + "/"
    val testPath = completePath + "/" + evalName + "/"
    val result = "/result/"

    val filterFeatures = Source.fromFile(new File(completePath + "/buildAbleNoFeatures")).getLines().toList
    val allFeaturesFile = completePath + "/allFeatures"
    val allFeatures = getAllFeaturesFromUniqueFeatureFile
    val pairWiseFeaturesFile = sourcePath + "/openssl_pairwise_configs.csv"
    val existingConfigsDir: String = completePath + "/existing_configs/"

    val featureModel: String = sourcePath + "/featuremodel"
    val featureModel_DIMACS: String = sourcePath + "/OpenSSL.dimacs"

    val runTimeout = 300000

    val FORCE_VARIABILITY = true
    val FORCE_LINKING = false

    val openSSLNoFeaturePrefix = "OPENSSL_NO"
    val openSSLFeaturePrefix = "OPENSSL_"
    val buildSystem = "linux-x86_64"
}
