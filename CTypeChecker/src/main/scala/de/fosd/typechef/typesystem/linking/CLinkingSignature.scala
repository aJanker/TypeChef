package de.fosd.typechef.typesystem.linking

import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.typesystem.linker.CSignature


case class CLinkingName(name: String, file: String) {}

object CLinkingName {
    def apply(i: Id) = new CLinkingName(i.name, i.getFile.getOrElse("$NO_FILE$"))
}

abstract class CLinkingSignature(declPos: Seq[CLinkingName], signature: CSignature) {
    def apply(declHeader: Seq[CLinkingName], classicSignature: CSignature): CLinkingSignature
}

case class ImportSignature(declPos: Seq[CLinkingName], classicSignature: CSignature) extends CLinkingSignature(declPos, classicSignature) {
    override def apply(declHeader: Seq[CLinkingName], classicSignature: CSignature): CLinkingSignature = ImportSignature(declPos, classicSignature)
}

case class ExportSignature(declPos: Seq[CLinkingName], classicSignature: CSignature) extends CLinkingSignature(declPos, classicSignature) {
    override def apply(declHeader: Seq[CLinkingName], classicSignature: CSignature): CLinkingSignature = ExportSignature(declPos, classicSignature)
}