package de.fosd.typechef.typesystem.modulelinking

import java.io.{File, Writer}

import de.fosd.typechef.error.Position
import de.fosd.typechef.typesystem.linker.CSignature

trait CLinkMapWriter {

    /**
      * Writes an XML-Version of the current link map to a specific writer.
      */
    def write(writer: Writer, map: CLinkMap): Unit = scala.xml.XML.write(writer, toXML(map), "UTF-8", xmlDecl = true, null)


    /**
      * Converts a linking map to XML.
      */
    def toXML(map: CLinkMap): xml.Elem =
        <map>
            <exports>
                {map.exportsByName.foreach(exportEntry => {
                <export>
                    <name>
                        {exportEntry._1}
                    </name>
                </export>
            })}
            </exports>
            <imports>
            </imports>
        </map>

    private def fileSignatureMapToXML(fileSignatureMap: Map[String, List[CLinkingSignature]]): xml.Elem =
        <link>

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
    def readFromFile(file: String): CLinkMap = readFromXML(xml.XML.loadFile(new File(file)))

    /**
      * Converts a linking information in xml to a CLinkMap
      */
    def readFromXML(xML: xml.Elem): CLinkMap = new CLinkMap()
}
