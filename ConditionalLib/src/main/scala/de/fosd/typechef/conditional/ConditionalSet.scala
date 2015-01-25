package de.fosd.typechef.conditional

import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory.{False, True}

/**
 * maintains a map
 * a name may be mapped to alternative entries with different feature expressions
 */
class ConditionalSet[A](private val entries: Map[A, FeatureExpr]) {
    def this() = this(Map())

    def ++(that: ConditionalSet[A]) = {
        val newMap = for (key <- (this.entries.keys ++ that.entries.keys)) yield
            (key -> (this.entries.getOrElse(key, False) or that.entries.getOrElse(key, False)))
        new ConditionalSet(newMap.toMap)
    }

    def union = ++ _
    def +(key: A, f: FeatureExpr) = new ConditionalSet[A](this.entries.+(key -> (f or this.entries.getOrElse(key, False))))
    def keys = entries.keys

    def get(key : A) : FeatureExpr = {
        this.entries.getOrElse(key, False)
    }

    def contains(name: A): FeatureExpr = this.entries.getOrElse(name, False)
    def isEmpty = entries.isEmpty

    /**
     * restricts the feature expression of all entries
     */
    def and(f: FeatureExpr): ConditionalSet[A] = new ConditionalSet(entries.mapValues(_ and f))

    override def equals(that: Any) = that match {
        case c: ConditionalSet[_] => entries equals c.entries;
        case _ => false
    }
    override def hashCode = entries.hashCode
    override def toString = entries.toString

    def toPlainSet(): Set[A] = {
        System.err.print(">>> remove me!")
        entries.keys.toSet
    }

    def toPlainSetWithConditionals() : Set[(A, FeatureExpr)] = {
        entries.map{ case (k,v) => (k,v) }.toSet
    }
}

object ConditionalSet {
    def apply[A]() = new ConditionalSet[A]()
    def apply[A](map : Map[A, FeatureExpr]) = new ConditionalSet[A](map)
    def apply[A](key: A, f: FeatureExpr) = new ConditionalSet[A](Map((key -> f)))
}

