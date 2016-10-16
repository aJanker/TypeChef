package de.fosd.typechef.parser.c

import de.fosd.typechef.conditional.{One, Opt}
import de.fosd.typechef.error.WithPosition

trait ASTRewriting extends org.bitbucket.inkytonik.kiama.rewriting.CallbackRewriter {

    /**
      * Rewriting ast nodes with kiama causes the loss of position informations.
      * By using kiama's builtin callback function, we are able to keep this information on every new term.
      */
    override def rewriting[T](oldTerm: T, newTerm: T): T = {
        (oldTerm, newTerm) match {
            case (source: WithPosition, target: WithPosition) => target.range = source.range
            case _ =>
        }
        newTerm
    }

    def checkPositionInformation(ast: Product) {
        val nodeswithoutposition: List[Product] = getNodesWithoutPositionInformation(ast)
        assert(nodeswithoutposition.size <= 1,
            "AST nodes with empty position information found (" + nodeswithoutposition.size + "): " +
                nodeswithoutposition.take(3).map(_.toString.take(200)).mkString("\n"))
    }

    // filter AST nodes that do not have position information
    private def getNodesWithoutPositionInformation[T <: Product](ast: T): List[AST] = {
        assert(ast != null, "ast should not be null")
        var nodeswithoutposition: List[AST] = List()

        val checkpos = everywherebu(query[Product] {
            case a: AST => if (!a.hasPosition) nodeswithoutposition ::= a
        })

        checkpos(ast)

        nodeswithoutposition
    }
}
object ASTRewriter extends ASTRewriting {
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

        val removeDead = manytd(rule[Product] {
            case l: List[_] => l.filter({
                case x: Opt[_] => env.featureExpr(x).isSatisfiable()
                case _ => true
            })
        })

        removeDead(ast).get.asInstanceOf[T]
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
