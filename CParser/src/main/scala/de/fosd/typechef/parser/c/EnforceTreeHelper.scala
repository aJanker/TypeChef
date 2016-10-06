package de.fosd.typechef.parser.c

import de.fosd.typechef.conditional.{One, Opt}


/**
 * preparation and checks for downstream tools
 * which require a tree structure
 *
 * we use the product interface of the elements here works both for
 * case classes Opt and AST elements, which derive product directly
 */
trait EnforceTreeHelper extends ASTRewriting {

    // creates an AST without shared objects
    // the parser reuses parsed elements in different subtrees of the AST
    // this method makes sure we create an AST with unique elements
    def prepareAST[T <: Product](ast: T): T = {
        assert(ast != null, "ast should not be null")

        val clone = everywherebu(rule[Product] {
            //            // function to add a break expression to infinite loops: "for (;;) {}" and "for (;;) ;"
            //            // reason is: for (;;) is the only infinite loop without explicit break statement,
            //            // so if we omit CompoundStatement in succ pred determination, we need an expression
            //            // so that succ(e) -> e and pred(e) is e
            //            // we add a Constant("1") at the break
            case ForStatement(None, None, None, One(CompoundStatement(List()))) =>
                ForStatement(None, Some(Constant("1")), None, One(CompoundStatement(List())))
            case n: AST => n.clone()
        })
        clone(ast).get.asInstanceOf[T]
    }

    // cparser creates dead ast nodes that causes problems in the control flow analysis (grouping of ast nodes etc.)
    // the function removes dead nodes from the ast
    // see issue: https://github.com/ckaestne/TypeChef/issues/4
    def removeDeadNodes[T <: Product](ast: T, env: ASTEnv): T = {
        assert(ast != null, "ast should not be null")

        val removedead = manytd(rule[Product] {
            case l: List[_] => l.filter({
                case x: Opt[_] => env.featureExpr(x).isSatisfiable()
                case _ => true
            })
        })

        removedead(ast).get.asInstanceOf[T]
    }

    // function to add a break expression to infinite loops: "for (;;) {}" and "for (;;) ;"
    // reason is: for (;;) is the only infinite loop without explicit break statement,
    // so if we omit CompoundStatement in succ pred determination, we need an expression
    // so that succ(e) -> e and pred(e) is e
    // we add a Constant("1") at the break
    def rewriteInfiniteForLoops[T <: Product](ast: T): T = {
        assert(ast != null, "ast should not be null")

        val rewrite = everywherebu(rule[Product] {
            case f@ForStatement(_, None, _, _) =>
                f.copy(expr2 = Some(Constant("1")))
            case n: AST => n
        })

        rewrite(ast).get.asInstanceOf[T]
    }
}
