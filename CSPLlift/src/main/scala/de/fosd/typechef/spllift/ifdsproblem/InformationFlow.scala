package de.fosd.typechef.spllift.ifdsproblem

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, Id, PrettyPrinter}

import scala.collection.mutable.ListBuffer


sealed trait InformationFlow extends Product with Cloneable {
    override def clone(): InformationFlow.this.type = super.clone().asInstanceOf[InformationFlow.this.type]
}

case object Zero extends InformationFlow

sealed abstract class Source(val name: Opt[Id], val stmt: Opt[_], val reachingSources: ListBuffer[Source] = ListBuffer(), val globalFile: Option[String] = None) extends InformationFlow {
    override def hashCode() = name.hashCode + stmt.hashCode() + globalFile.hashCode()
}

case class VarSource(override val name: Opt[Id], override val stmt: Opt[_], override val reachingSources: ListBuffer[Source] = ListBuffer(), override val globalFile: Option[String] = None) extends Source(name, stmt, reachingSources, globalFile) {
    override def equals(other: Any) = other match {
        case s@VarSource(oName, oStmt, _, oGlobalFile) => oName.equals(name) && oStmt.equals(stmt) && oGlobalFile.equals(globalFile)
        case _ => false
    }
}

case class StructSource(override val name: Opt[Id], field: Option[Source], override val stmt: Opt[_], override val reachingSources: ListBuffer[Source] = ListBuffer(), override val globalFile: Option[String] = None) extends Source(name, stmt, reachingSources, globalFile) {
    override def equals(other: Any) = other match {
        case s@StructSource(oName, _, oStmt, _, oGlobalFile) => oName.equals(name) && oStmt.equals(stmt) && oGlobalFile.equals(globalFile)
        case _ => false
    }
}

// TODO to implement
case class PointerSource(override val name: Opt[Id], override val stmt: Opt[_], override val reachingSources: ListBuffer[Source] = ListBuffer(), override val globalFile: Option[String] = None) extends Source(name, stmt, reachingSources, globalFile) {
    override def equals(other: Any) = other match {
        case s@PointerSource(oName, oStmt, _, oGlobalFile) => oName.equals(name) && oStmt.equals(stmt) && oGlobalFile.equals(globalFile)
        case _ => false
    }
}


case class Reach(to: Opt[AST], from: List[Opt[Id]], sources: List[Source]) extends InformationFlow {
    def toText: String = {
        val builder = new StringBuilder
        builder.append("Reach under condition " + to.condition.toTextExpr + " at " + PrettyPrinter.print(to.entry) + "\n")

        if (from.nonEmpty) {
            builder.append("\tFrom:\t")
            from.foreach(entry => builder.append(entry.entry.name + " when " + entry.condition.toTextExpr + ";\t"))
        }

        if (sources.nonEmpty) builder.append("\n\tSources: " + sources)
        builder.toString
    }
}

