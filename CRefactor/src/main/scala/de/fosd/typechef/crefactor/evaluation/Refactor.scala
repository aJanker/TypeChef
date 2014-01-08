package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking
import de.fosd.typechef.crefactor.Morpheus

trait Refactoring {

    def refactor(morpheus: Morpheus, linkInterface: CLinking): (Boolean, AST, List[FeatureExpr], List[(String, AST)])

}

trait Refactor {

    def rename(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking)

    def extract(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking)

    def inline(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking)
}


