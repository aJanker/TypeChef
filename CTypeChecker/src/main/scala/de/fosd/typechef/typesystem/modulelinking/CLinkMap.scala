package de.fosd.typechef.typesystem.modulelinking

import scala.collection.immutable.HashMap
import scala.collection.mutable

class CLinkMap(exports: List[ExportSignature] = List(), imports: List[ImportSignature] = List()) {

    type LinkName = String
    type File = String
    type ExportSignatures = List[ExportSignature]
    type ImportSignatures = List[ImportSignature]

    private[modulelinking] def getExports = exports
    private[modulelinking] def getImports = imports

    private[modulelinking] val exportsByName: mutable.Map[LinkName, Map[File, ExportSignatures]] = new mutable.HashMap[LinkName, Map[File, ExportSignatures]]()
    private[modulelinking] val importsByName: mutable.Map[LinkName, Map[File, ImportSignatures]] = new mutable.HashMap[LinkName, Map[File, ImportSignatures]]()

    def getAllImportedNames: List[(LinkName, Map[File, ImportSignatures])] = importsByName.toList

    fillMap()

    def getExportsByName(name: LinkName): Option[Map[File, ExportSignatures]] = exportsByName.get(name)
    def getImportsByName(name: LinkName): Option[Map[File, ImportSignatures]] = importsByName.get(name)

    def merge(other: CLinkMap): CLinkMap = new CLinkMap(getExports ::: other.getExports, getImports ::: other.getImports)

    private def fillMap() = {
        def emptyFileMap[T <: CLinkingSignature] = new HashMap[File, List[T]]()
        def emptySigList[T <: CLinkingSignature] = List[T]()

        def doFill[T <: CLinkingSignature](linkedSig: List[T], linkMap: mutable.Map[LinkName, Map[File, List[T]]]): Unit =
            linkedSig.foreach(linkEntry => linkEntry.getDeclNames.foreach(x => {
                val (name, file) = (x.name, x.file)
                val fileMapEntry = linkMap.getOrElse(name, emptyFileMap[T])
                val updatedSigs = (linkEntry :: fileMapEntry.getOrElse(file, emptySigList[T])).distinct

                linkMap.put(name, fileMapEntry + (file -> updatedSigs))
            }))


        doFill(getExports, exportsByName)
        doFill(getImports, importsByName)
    }
}

object CLinkMap extends CLinkMapWriter with CLinkMapReader {}
