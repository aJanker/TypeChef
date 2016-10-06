package de.fosd.typechef.parser.c

import de.fosd.typechef.error.WithPosition

trait ASTRewriting extends org.kiama.rewriting.CallbackRewriter {

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