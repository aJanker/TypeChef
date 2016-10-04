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

}
