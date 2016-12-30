package de.fosd.typechef.typesystem.linking

import scala.collection.mutable

class CLinkMap(exports : List[ExportSignature] = List(), imports: List[ImportSignature] = List()) {

    private val exportsByName : mutable.Map[String, Map[String, ExportSignature]] = new mutable.HashMap[String, Map[String, ExportSignature]]()
    private val importsByName : mutable.Map[String, Map[String, ImportSignature]] = new mutable.HashMap[String, Map[String, ImportSignature]]()

    fillMap()

    def getExportsByName(name : String) : Option[Map[String, ExportSignature]] = exportsByName.get(name)
    def getImportsByName(name : String) : Option[Map[String, CLinkingSignature]] = importsByName.get(name)

    def merge(other : CLinkMap) : CLinkMap = new CLinkMap(this.exports ::: other.exports, this.imports ::: other.imports)

    // fill map
    private def fillMap() = ???
}
