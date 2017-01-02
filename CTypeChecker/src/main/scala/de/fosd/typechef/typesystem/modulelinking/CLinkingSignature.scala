package de.fosd.typechef.typesystem.modulelinking

import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.typesystem.linker.CSignature


case class CLinkingName(name: String, file: String) {}

object CLinkingName {
    def apply(i: Id) = new CLinkingName(i.name, i.getFile.getOrElse("$NO_FILE$"))
}

abstract class CLinkingSignature(declNames: Seq[CLinkingName], signature: CSignature) {
    def apply(declHeader: Seq[CLinkingName], classicSignature: CSignature): CLinkingSignature
    def getDeclNames: Seq[CLinkingName] = declNames
    def getSignature: CSignature = signature
}

case class ImportSignature(declNames: Seq[CLinkingName], classicSignature: CSignature) extends CLinkingSignature(declNames, classicSignature) {
    override def apply(declHeader: Seq[CLinkingName], classicSignature: CSignature): CLinkingSignature = ImportSignature(declNames, classicSignature)
}

case class ExportSignature(declNames: Seq[CLinkingName], classicSignature: CSignature) extends CLinkingSignature(declNames, classicSignature) {
    override def apply(declHeader: Seq[CLinkingName], classicSignature: CSignature): CLinkingSignature = ExportSignature(declNames, classicSignature)
}