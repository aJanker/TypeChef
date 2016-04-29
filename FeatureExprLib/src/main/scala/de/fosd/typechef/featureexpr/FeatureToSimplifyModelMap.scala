package de.fosd.typechef.featureexpr

import java.io.File
import java.util
import java.util.Collections

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.io.Source


object FeatureToSimplifyModelMap {

    private lazy val featureToSimpleModelMap: mutable.HashMap[String, List[String]] = new HashMap[String, List[String]]
    private lazy val itemIdentifier : String = "#item "

    def fill(simplifyFM : File) : mutable.HashMap[String, List[String]] = {
        if (featureToSimpleModelMap.nonEmpty) clear()

        var currentName : String = null

        for (line <- Source.fromFile(simplifyFM).getLines) {
            if (line.startsWith(itemIdentifier)) currentName = line.substring(itemIdentifier.length)
            else add(currentName, line)
        }

        featureToSimpleModelMap
    }

    def get() : mutable.HashMap[String, List[String]] = featureToSimpleModelMap

    def clear() = featureToSimpleModelMap.clear

    private def add(name: String, fExpr : String) =
        featureToSimpleModelMap.get(name) match {
            case Some(l) => featureToSimpleModelMap + (name -> (fExpr :: l))
            case _ => featureToSimpleModelMap + (name -> List(fExpr))
        }

}


