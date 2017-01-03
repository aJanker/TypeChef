package de.fosd.typechef.typesystem.modulelinking

import java.io.{File, FileWriter, Writer}

import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.FeatureExprParser
import de.fosd.typechef.typesystem.CType
import de.fosd.typechef.typesystem.linker.{CFlag, CSignature, WeakExport}

trait CLinkMapWriter {

    /**
      * Writes an XML-Version of the current link map to a specific writer.
      */
    def write(writer: Writer, map: CLinkMap): Unit = scala.xml.XML.write(writer, toXML(map), "UTF-8", xmlDecl = true, null)

    def writeToFile(file: String, map: CLinkMap): Unit = {
        val writer = new FileWriter(file)
        write(writer, map)
        writer.close()
    }

    /**
      * Converts a linking map to XML.
      */
    def toXML(map: CLinkMap): xml.Elem =
        <map>
            <exports>
                {map.getExports.map(entry => {
                <export>
                    <decl>
                        {entry.declNames.map(linkingNameToXML)}
                    </decl>{signatureToXML(entry.classicSignature)}
                </export>
            })}
            </exports>
            <imports>
                {map.getImports.map(entry => {
                <import>
                    <decl>
                        {entry.declNames.map(linkingNameToXML)}
                    </decl>{signatureToXML(entry.classicSignature)}
                </import>
            })}
            </imports>
        </map>

    private def linkingNameToXML(linkName: CLinkingName): xml.Elem =
        <link>
            <name>
                {linkName.name}
            </name>
            <file>
                {linkName.file}
            </file>
        </link>

    private def signatureToXML(sig: CSignature): xml.Elem =
        <sig>
            <name>
                {sig.name}
            </name>
            <type>
                {sig.ctype.toXML}
            </type>
            <featureexpr>
                {sig.fexpr.toTextExpr}
            </featureexpr>{sig.pos.map(posToXML)}{for (extraFlag <- sig.extraFlags) yield <extraFlag name={extraFlag.toString}/>}
        </sig>

    private def posToXML(p: Position) =
        <pos>
            <file>
                {p.getFile}
            </file>
            <line>
                {p.getLine}
            </line>
            <col>
                {p.getColumn}
            </col>
        </pos>

}

trait CLinkMapReader {

    /**
      * Reads in a linking map from a XML file.
      */
    def readFromFile(file: String): CLinkMap = fromXML(xml.XML.loadFile(new File(file)))

    /**
      * Converts linking information from xml to a CLinkMap
      */
    def fromXML(node: xml.Elem): CLinkMap = {
        def _FromXML[T <: CLinkingSignature](parentNode: String, entryNode: String, apply: (Seq[CLinkingName], CSignature) => T): List[T] =
            (node \ parentNode).flatMap(childNode => (childNode \ entryNode).map(entry => {
                val linkNames = (entry \ "decl").map(linkNameFromXML)
                val sig = signatureFromXML(entry)
                apply(linkNames, sig)
            })).toList

        new CLinkMap(_FromXML("exports", "export", ExportSignature.apply), _FromXML("imports", "import", ImportSignature.apply))
    }

    private def linkNameFromXML(node: scala.xml.Node): CLinkingName = {
        val link = node \ "link"
        val name = (link \ "name").text.trim
        val file = (link \ "file").text.trim
        CLinkingName(name, file)
    }


    private def signatureFromXML(node: scala.xml.Node): CSignature = {
        val sig = node \ "sig"
        CSignature(
            (sig \ "name").text.trim,
            CType.fromXML(sig \ "type"),
            new FeatureExprParser().parse((sig \ "featureexpr").text),
            (sig \ "pos").map(positionFromXML),
            (sig \ "extraFlag").flatMap(extraFlagFromXML).filter(_.isDefined).map(_.get).toSet
        )
    }
    private def positionFromXML(node: scala.xml.Node): Position = {
        val col = (node \ "col").text.trim.toInt
        val line = (node \ "line").text.trim.toInt
        val file = (node \ "file").text.trim
        new Position() {
            def getColumn: Int = col
            def getLine: Int = line
            def getFile: String = file
        }
    }

    private def extraFlagFromXML(node: scala.xml.Node): Seq[Option[CFlag]] = {
        (node \ "@name").map(n => if (n.text == "WeakExport") Some(WeakExport) else None)
    }
}
