package de.fosd.typechef.crewrite

import de.fosd.typechef.conditional.{Choice, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c.{AST, ASTRewriting}

object ProductDerivation extends ASTRewriting {

    def deriveProduct[T <: Product](ast: T, selectedFeatures: Set[String], condition: FeatureExpr = FeatureExprFactory.True): T = {
        val prod = manytd(rule[Product] {
            case l: List[_] if l.forall(_.isInstanceOf[Opt[_]]) =>
                l.flatMap {
                    case o: Opt[_] if o.condition.evaluate(selectedFeatures) => Some(o.copy(condition = condition))
                    case _ => None
                }
            case Choice(feature, thenBranch, elseBranch) =>
                if (feature.evaluate(selectedFeatures)) thenBranch
                else elseBranch
            case a: AST => a.clone()
        })

        val cast = prod(ast).get.asInstanceOf[T]
        checkPositionInformation(cast)
        cast
    }
}
